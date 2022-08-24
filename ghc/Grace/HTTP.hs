{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns     #-}

{-| This module provides a uniform interface for making HTTP requests using both
    GHC and GHCJS
-}
module Grace.HTTP
    ( HttpException
    , Manager
    , newManager
    , fetch
    , fetchWithBody
    , renderError
    , FetchCache
    ) where

import Control.Exception (Exception(..))
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Network.HTTP.Client (HttpExceptionContent(..), Manager)

import qualified Control.Exception as Exception
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Lazy.Encoding
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types as HTTP.Types

-- | Exception type thrown by `fetch` in the event of any failure
data HttpException
    = HttpException HTTP.HttpException
    | NotUTF8 UnicodeException
    deriving stock (Show)

instance Exception HttpException where
    displayException = Text.unpack . renderError

-- | Acquire a new `Manager`
newManager :: IO Manager
newManager = TLS.newTlsManager

type FetchCache = Map.Map (Text, Text) Text

-- | Fetch a URL (using the @http-client@ package)
fetch
    :: Manager
    -> Text
    -- ^ URL
    -> IO Text
    -- ^ Response body
fetch manager url = do
    request <- HTTP.parseUrlThrow (Text.unpack url)

    let handler :: HTTP.HttpException -> IO a
        handler httpException = Exception.throwIO (HttpException httpException)

    response <- Exception.handle handler (HTTP.httpLbs request manager)

    let lazyBytes = HTTP.responseBody response

    case Lazy.Encoding.decodeUtf8' lazyBytes of
        Left exception -> Exception.throwIO (NotUTF8 exception)
        Right lazyText -> return (Text.Lazy.toStrict lazyText)


-- | Post to a URL (using the @http-client@ package)
fetchWithBody
    :: Manager
    -> Text
    -- ^ URL
    -> Text
    -- ^ Request Body
    -> Maybe (MVar.MVar FetchCache)
    -> IO Text
    -- ^ Response body
fetchWithBody manager url requestBody cache = do
    request <- HTTP.parseUrlThrow (Text.unpack url)
    let postRequest = request
          { HTTP.method = "POST"
          , HTTP.requestBody = HTTP.RequestBodyBS (Encoding.encodeUtf8 requestBody)
          }

    let handler :: HTTP.HttpException -> IO a
        handler httpException = Exception.throwIO (HttpException httpException)

    response <- Exception.handle handler (HTTP.httpLbs postRequest manager)

    let lazyBytes = HTTP.responseBody response

    case Lazy.Encoding.decodeUtf8' lazyBytes of
        Left exception -> Exception.throwIO (NotUTF8 exception)
        Right lazyText -> return (Text.Lazy.toStrict lazyText)


-- | Render an `HttpException` as `Text`
renderError :: HttpException -> Text
renderError (HttpException httpException) = case httpException of
    HTTP.InvalidUrlException _ _ ->
        "Invalid URL"

    HTTP.HttpExceptionRequest _ e -> case e of
        ConnectionFailure _ ->
            "Remote host not found"
        InvalidDestinationHost _ ->
            "Invalid remote host name"
        ResponseTimeout ->
            "The remote host took too long to respond"
        ConnectionTimeout ->
            "Connection establishment took too long"
        StatusCodeException response body -> prefix <> suffix
          where
            statusCode =
                HTTP.Types.statusCode (HTTP.responseStatus response)

            prefix =
                case statusCode of
                    401 -> "Access unauthorized"
                    403 -> "Access forbidden"
                    404 -> "Remote file not found"
                    500 -> "Server-side failure"
                    502 -> "Upstream failure"
                    503 -> "Server temporarily unavailable"
                    504 -> "Upstream timeout"
                    _   -> "HTTP request failure"

            suffix =
                    "\n\
                    \\n\
                    \HTTP status code: " <> Text.pack (show statusCode) <> responseBody

            responseBody :: Text
            responseBody =
                case Encoding.decodeUtf8' body of
                    Left _ ->
                            "\n\
                            \\n\
                            \Response body (non-UTF8 bytes):\n\
                            \\n\
                            \" <> Text.pack (show body)
                    Right "" ->
                        ""
                    Right bodyText ->
                            "\n\n"
                        <>  "Response body:\n\
                            \\n\
                            \" <> prefixedText
                      where
                        prefixedLines =
                                zipWith combine prefixes
                                    (Text.lines bodyText)
                            <>  [ "…│ …" ]
                          where
                            prefixes = [(1 :: Int)..7]

                            combine n line =
                                Text.pack (show n) <> "│ " <> line

                        prefixedText = Text.unlines prefixedLines
        _ ->
           "HTTP request failure\n\
           \\n\
           \" <> Text.pack (displayException httpException)
renderError (NotUTF8 unicodeException) =
    "Not UTF8\n\
    \\n\
    \" <> Text.pack (displayException unicodeException)
