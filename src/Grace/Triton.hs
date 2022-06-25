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
import Data.Traversable (forM)
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
  { tensorName :: Text
  , datatype :: DataType
  , shape :: [Int]
  , data_ :: [Float]
  }
  deriving (Eq, Show, Generic)

instance ToJSON TritonTensor where
  toJSON TritonTensor { tensorName, datatype, shape, data_ } = object
    [ "name" .= tensorName
    , "datatype" .= datatype
    , "shape" .= shape
    , "data" .= data_
    ]

instance FromJSON TritonTensor where
  parseJSON = withObject "TritonTensor" $ \o -> do
    tensorName <- o .: "name"
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

data RepositoryModel = RepositoryModel
  { modelName :: Text
  , version :: Text
  , state :: Text
  }
  deriving (Eq, Show)

instance FromJSON RepositoryModel where
  parseJSON = withObject "RepositoryModel" $ \o -> do
    modelName <- o .: "name"
    version <- o .: "version"
    state <- o .: "state"
    return RepositoryModel {..}

data ModelMetadata = ModelMetadata
  { mmName :: Text
  , mmInputs :: [TritonVectorType]
  , mmOutputs :: [TritonVectorType]
  }
  deriving (Eq, Show)

instance FromJSON ModelMetadata where
  parseJSON = withObject "ModelMetadata" $ \o -> do
    mmName <- o .: "name"
    mmInputs <- o .: "inputs"
    mmOutputs <- o .: "outputs"
    return ModelMetadata {..}

data TritonVectorType = TritonVectorType
  { tvtName :: Text
  , tvtDatatype :: DataType
  , tvtShape :: [Int]
  }
  deriving (Eq, Show)

instance FromJSON TritonVectorType where
  parseJSON = withObject "TritonVectorType" $ \o -> do
    tvtName <- o .: "name"
    tvtDatatype <- o .: "datatype"
    tvtShape <- o .: "shape"
    return TritonVectorType {..}
  
listModels :: Manager -> IO [ModelMetadata]
listModels manager = do
  resp <- HTTP.fetchWithBody manager "http://localhost:8000/v2/repository/index" ""
  let (Right (repositoryResponse :: [RepositoryModel])) = eitherDecode (fromStrict $ encodeUtf8 resp)
  print repositoryResponse
  let modelNames = modelName <$> repositoryResponse
  forM modelNames $ \modelName -> do
    modelResp <- HTTP.fetch manager ("http://localhost:8000/v2/models/" <> modelName)
    print modelResp
    let (Right modelMetadata) = eitherDecode . fromStrict . encodeUtf8 $ modelResp
    return modelMetadata


test :: IO ()
test = do
  manager <- HTTP.newManager
  let req = InferenceRequest
            { inputs = [TritonTensor
               { tensorName = "Input3"
               , datatype = FP32
               , shape = [1,1,28,28]
               , data_ = replicate (28 * 28) 1.0
               }]
            }
  listModels manager >>= print
  res <- infer manager "mnist" req
  print res
