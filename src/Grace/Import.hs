{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | This module contains the import resolution logic
module Grace.Import
    ( -- * Import resolution
      resolve
      -- * Exceptions
    , ResolutionError(..)
    , ImportError(..)
    ) where

import Control.Exception.Safe (Exception(..))
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Grace.HTTP (HttpException, Manager)
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import Grace.Syntax (Syntax)
import System.FilePath ((</>))
import Text.URI (Authority)

import qualified Control.Exception.Safe as Exception
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IORef as IORef
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Grace.HTTP as HTTP
import qualified Grace.Parser as Parser
import qualified Grace.Pretty as Pretty
import qualified System.Environment as Environment
import qualified System.IO.Unsafe as Unsafe
import qualified Text.URI as URI


#ifdef ghcjs_HOST_OS
import qualified Data.JSString as JSString
#endif

#ifdef ghcjs_HOST_OS
cache :: IORef (HashMap Text JSString.JSString)
#else
cache :: IORef (HashMap Text Text)
#endif
cache = Unsafe.unsafePerformIO (IORef.newIORef HashMap.empty)
{-# NOINLINE cache #-}

fetch :: Manager -> Text -> IO Text
fetch manager url = do
    m <- IORef.readIORef cache

    body <- case HashMap.lookup url m of
        Nothing -> do
            body  <- HTTP.fetch manager url
            IORef.writeIORef cache $! HashMap.insert url body m
            return body
        Just body -> do
            return body
#ifdef ghcjs_HOST_OS
    return (Text.pack (JSString.unpack body))
#else
    return body
#endif

-- | Resolve an `Input` by returning the source code that it represents
resolve :: Manager -> Input -> IO (Syntax Location Input)
resolve manager input = case input of
    URI uri
        | let schemes = map (fromJust . URI.mkScheme) [ "http", "https" ]
        , any (`elem` schemes) (URI.uriScheme uri) -> do
            let name = URI.renderStr uri

            let handler e = throw (HTTPError e)

            code <- Exception.handle handler (fetch manager (Text.pack name))

            result <- case Parser.parse name code of
                Left e -> Exception.throw e
                Right result -> return result

            let locate offset = Location{..}

            return (first locate result)

        | URI.uriScheme uri == URI.mkScheme "env" -> do
            case URI.uriAuthority uri of
                Left False -> do
                    var <- case URI.uriPath uri of
                        Nothing -> throw MissingPath
                        Just (False, var :| []) -> return (URI.unRText var)
                        _ -> throw UnsupportedPathSeparators

                    maybeCode <- Environment.lookupEnv (Text.unpack var)

                    code <- case maybeCode of
                        Nothing -> throw MissingEnvironmentVariable
                        Just string -> return (Text.pack string)

                    let name = "env:" <> Text.unpack var

                    result <- case Parser.parse name code of
                        Left e -> Exception.throw e
                        Right result -> return result

                    let locate offset = Location{..}

                    return (first locate result)
                Left True -> do
                    throw UnsupportedPathSeparators
                Right _ -> do
                    throw UnsupportedAuthority

        | URI.uriScheme uri == URI.mkScheme "file" -> do
            if all (== emptyAuthority) (URI.uriAuthority uri)
                then do
                    pieces <- case URI.uriPath uri of
                        Nothing -> throw MissingPath
                        Just (_, pieces) -> return pieces

                    let pathPiecesToFilePath =
                            foldl' (</>) "/" . map (Text.unpack . URI.unRText) . NonEmpty.toList

                    readPath (pathPiecesToFilePath pieces)
                else do
                    throw UnsupportedAuthority

        | otherwise -> do
            throw InvalidURI

    Path path -> do
        readPath path

    Code name code -> do
        result <- case Parser.parse name code of
            Left e -> Exception.throw e
            Right result -> return result

        let locate offset = Location{..}

        return (first locate result)
  where
    readPath path = do
        code <- Text.IO.readFile path

        result <- case Parser.parse path code of
            Left e -> Exception.throw e
            Right result -> return result

        let locate offset = Location{ name = path, ..}

        return (first locate result)

    throw e = Exception.throw (ImportError input e)

emptyAuthority :: Authority
emptyAuthority = URI.Authority
    { URI.authUserInfo = Nothing
    , URI.authHost = fromJust (URI.mkHost "")
    , URI.authPort = Nothing
    }

-- | The base error for `ImportError` (without the @input@ information)
data ResolutionError
    = HTTPError HttpException
    | InvalidURI
    | MissingEnvironmentVariable
    | MissingPath
    | UnsupportedPathSeparators
    | ReferentiallyInsane Input
    | UnsupportedAuthority
    deriving stock (Show)

-- | Errors related to import resolution
data ImportError = ImportError
    { input :: Input
    , resolutionError :: ResolutionError
    } deriving stock (Show)

instance Exception ImportError where
    displayException ImportError{..} =
        Text.unpack
            ("Import resolution failed: " <> renderedInput <> "\n\n" <> renderedError)
      where
        renderedInput = case input of
            URI  uri  -> URI.render uri
            Path path -> Text.pack path
            Code _ _  -> "(input)"

        renderedError :: Text
        renderedError = case resolutionError of
            HTTPError httpException ->
                HTTP.renderError httpException
            InvalidURI ->
                "Invalid URI"
            MissingEnvironmentVariable ->
                "Missing environment variable"
            MissingPath ->
                "Missing path"
            ReferentiallyInsane child ->
                "Local imports are rejected within remote imports\n\nRejected local import: " <> Text.pack (show (Pretty.pretty child))
            UnsupportedPathSeparators ->
                "Unsupported path separators"
            UnsupportedAuthority ->
                "Unsupported authority"
