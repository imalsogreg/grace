-- | 

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grace.Triton where

import Data.List (replicate)
import Data.ByteString.Lazy( fromStrict, toStrict )
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text, unpack)
import Data.Aeson (eitherDecode, withObject, (.:), encode, Value, object, FromJSON(..), ToJSON(..), (.=), withText)
import GHC.Generics
import Grace.HTTP as HTTP

-- * Inference

infer :: HTTP.Manager -> Text -> InferenceRequest -> IO InferenceResponse
infer manager modelName inferenceRequest = do
  let url = "http://localhost:8000/v2/models/" <> modelName <> "/infer"
  res <- HTTP.fetchWithBody manager url (toStrict $ encode inferenceRequest)
  let (Right inferenceResponse) = eitherDecode . fromStrict $ encodeUtf8 res
  return inferenceResponse

data InferenceRequest = InferenceRequest
  { inputs :: [TritonTensor] }
  deriving (Eq, Show, Generic)

data InferenceResponse = InferenceResponse
  { outputs :: [TritonTensor] }
  deriving (Eq, Show, Generic)

instance ToJSON InferenceRequest where
instance FromJSON InferenceResponse where

data TritonTensor = TritonTensor
  { name :: Text
  , datatype :: DataType
  , shape :: [Int]
  , data_ :: [Float]
  }
  deriving (Eq, Show, Generic)

instance ToJSON TritonTensor where
  toJSON TritonTensor { name, datatype, shape, data_ } = object
    [ "name" .= name
    , "datatype" .= datatype
    , "shape" .= shape
    , "data" .= data_
    ]

instance FromJSON TritonTensor where
  parseJSON = withObject "TritonTensor" $ \o -> do
    name <- o .: "name"
    datatype <- o .: "datatype"
    shape <- o .: "shape"
    data_ <- o .: "data"
    return TritonTensor{..}

data DataType =
  FP32
  deriving (Eq, Show)

instance ToJSON DataType where
  toJSON d = case d of
    FP32 -> toJSON ("FP32" :: String)

instance FromJSON DataType where
  parseJSON = withText  "DataType"$ \(txt :: Text) -> case txt of
    "FP32" -> pure FP32
    _ -> fail ("Invalid DataType: " <> unpack txt)

-- * Model Reflection (TOOD)

-- data Model

test :: IO ()
test = do
  manager <- HTTP.newManager
  let req = InferenceRequest
            { inputs = [TritonTensor
               { name = "Input3"
               , datatype = FP32
               , shape = [1,1,28,28]
               , data_ = replicate (28 * 28) 1.0
               }]
            }
  res <- infer manager "mnist" req
  print res
