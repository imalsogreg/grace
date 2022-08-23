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
    ) where

import Control.Exception (Exception(..))
import qualified Control.Exception as Exception
import Data.Text (Text)
import GHCJS.Fetch (Request(..), JSPromiseException)
import Data.JSString (JSString)
import GHCJS.Marshal (toJSVal)
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

-- | Fetch a URL (using @XMLHttpRequest@)
fetch
    :: Manager
    -> Text
    -- ^ URL
    -> IO Text
    -- ^ Response body
fetch _manager url = do
    let request = Request
            { reqUrl = JSString.pack (Text.unpack url)
            , reqOptions = Fetch.defaultRequestOptions
            }

    response <- Fetch.fetch request

    jsString <- Fetch.responseText response

    return (Text.pack (JSString.unpack jsString))

fetchWithBody
    :: Manager
    -> Text
    -> Text
    -- ^ Request body
    -> IO Text
    -- ^ Response body
fetchWithBody _manager url requestBody = do
    -- let reqBodyJSString = JSString.pack (BS.unpack requestBody)
    reqBodyJSString <- (toJSVal requestBody)
    consoleLog "evaluate request"
    request <- Exception.evaluate $
          Request
            { reqUrl = JSString.pack (Text.unpack url)
            , reqOptions = Fetch.defaultRequestOptions { Fetch.reqOptMethod = "POST"
                                                 , Fetch.reqOptBody = Just reqBodyJSString
                                                 }
            }
    consoleLog "done evaluating request"
    consoleLog ("about to fetch")
    resp <- Fetch.fetch request
    jsString <- Fetch.responseText resp
    consoleLog ("finished fetch")
    return (Text.pack (JSString.unpack jsString))


-- | Render an `HttpException` as `Text`
renderError :: HttpException -> Text
renderError = Text.pack . displayException


consoleLog :: Text -> IO ()
consoleLog t = consoleLog_ (JSString.pack $ Text.unpack t)

foreign import javascript unsafe "console.log($1)"
  consoleLog_ :: JSString -> IO ()
