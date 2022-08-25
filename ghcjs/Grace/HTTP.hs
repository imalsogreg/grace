{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

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
import qualified Control.Exception as Exception
import Data.Text (Text)
import Data.Maybe (fromMaybe, maybe)
import qualified Data.Map as Map
import GHCJS.Fetch (Request(..), JSPromiseException)
import Data.JSString (JSString)
import GHCJS.Marshal (toJSVal)
import qualified Control.Concurrent.MVar as MVar
import GHCJS.Marshal.Pure (pToJSVal)

import qualified Data.JSString as JSString
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified GHCJS.Fetch as Fetch

{-| The GHCJS implementation of HTTP requests doesn't require a real `Manager`
    so this supplies an empty placeholder
-}
type Manager = ()

-- | An `HttpException` is just a type synonym for a `JSPromiseException`
type HttpException = JSPromiseException

{-| Acquire a new `Manager`

    This does nothing since the GHCJS implementation doesn't use a `Manager`
-}
newManager :: IO Manager
newManager = mempty

type FetchCache = Map.Map (Text, Text) Text

-- | Fetch a URL (using @XMLHttpRequest@)
fetch
    :: Manager
    -> Text
    -- ^ URL
    -> IO JSString.JSString
    -- ^ Response body
fetch _manager url = do
    let request = Request
            { reqUrl = JSString.pack (Text.unpack url)
            , reqOptions = Fetch.defaultRequestOptions
            }

    response <- Fetch.fetch request

    Fetch.responseText response

fetchWithBody
    :: Manager
    -> Text
    -- ^ URL
    -> JSString.JSString
    -- ^ Request body
    -> Maybe (MVar.MVar FetchCache)
    -> IO JSString.JSString
    -- ^ Response body
fetchWithBody _manager url requestBody cacheMVar = do
    -- let reqBodyJSString = JSString.pack (BS.unpack requestBody)
    reqBodyJSString <- (toJSVal requestBody)
    consoleLog "evaluate request"
    let cacheKey = (url, requestBody)
    cache <- maybe (pure Map.empty) MVar.takeMVar cacheMVar
    -- case Map.lookup cacheKey cache of
    case True of
      False -> do
      -- Just resp -> do
        consoleLog "cache hit"
        maybe (pure ()) (\c -> MVar.putMVar c cache) cacheMVar
        -- return resp
        undefined
      -- Nothing -> do
      True -> do
        consoleLog "cache miss"

        request <- Exception.evaluate $
            Request
                { reqUrl = JSString.pack (Text.unpack url)
                , reqOptions = Fetch.defaultRequestOptions { Fetch.reqOptMethod = "POST"
                                                    , Fetch.reqOptBody = Just reqBodyJSString
                                                    }
                }
        consoleLog ("about to fetch")
        resp <- Fetch.fetch request
        jsString <- Fetch.responseText resp
        -- let respText = Text.pack (JSString.unpack jsString)
        consoleLog ("finished fetch")
        -- maybe (pure ()) (\c -> MVar.putMVar c (Map.insert cacheKey respText cache)) cacheMVar
        return jsString


-- | Render an `HttpException` as `Text`
renderError :: HttpException -> Text
renderError = Text.pack . displayException


consoleLog :: Text -> IO ()
consoleLog t = consoleLog_ (JSString.pack $ Text.unpack t)

foreign import javascript unsafe "console.log($1)"
  consoleLog_ :: JSString -> IO ()
