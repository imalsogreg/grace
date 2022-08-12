-- | 

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grace.Triton where

import Data.List (uncons, find)
import Data.Foldable (toList)
import Data.ByteString.Lazy( fromStrict, toStrict )
import Data.Traversable (forM)
import Data.Sequence (fromList)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text, unpack)
import Data.Aeson (eitherDecode, withObject, (.:), encode, object, FromJSON(..), ToJSON(..), (.=), withText)
import GHC.Generics
import Grace.HTTP as HTTP
import Grace.Type (Type(..))
import qualified Grace.Pretty as Pretty
import qualified Grace.Type as GraceType
import qualified Grace.Syntax as GraceSyntax
import qualified Grace.Value as GraceValue
import Grace.Location (Location(..), Offset(..))
import qualified Grace.Monotype as Monotype

-- * Triton Grace primitives

loadContext :: IO ( [(Text, GraceType.Type Location, GraceValue.Value)])
loadContext = do
  manager <- HTTP.newManager
  models <- listModels manager
  return $ tritonPrimitivesForModel <$> models

normalizeTritonCallApplication :: Text -> GraceValue.Value -> IO GraceValue.Value
normalizeTritonCallApplication prefixedModelName value = do
  manager <- HTTP.newManager

  -- We have to look up models from scratch because single-input and single-output
  -- values drop the name of the tensor.
  models <- listModels manager
  
  -- context <- loadContext -- TODO: It's pretty inefficient to do this every time we normalize an triton call.
  let

    -- This also only works for single-input, single-output models.
    Just (ModelMetadata { mmName
                        , mmInputs = [TritonTensorType{ tvtName = inputTensorName, tvtShape = inputTensorShape }]
                        , mmOutputs = [_modelOutput]
                        }) =
      find (\ModelMetadata {mmName = name} -> "triton_" <> name ==  prefixedModelName) models

    lowerElements :: GraceValue.Value -> [Float]
    lowerElements v = case v of
      GraceValue.Scalar ( GraceSyntax.Real r ) -> [realToFrac r]
      GraceValue.Scalar ( GraceSyntax.Natural r ) -> [realToFrac r] -- TODO: It seems like a bug that we need this - we _are_ seeing Nats.
      GraceValue.List (xs) -> concat $ lowerElements <$> xs
      _ -> error ("val: " <> show v)

    reifyElement v = GraceValue.Scalar (GraceSyntax.Real $ realToFrac v)

    lowerTensorValue :: GraceValue.Value -> TritonTensor
    lowerTensorValue t = case t of
      GraceValue.Tensor (Monotype.TensorShape shape) elements ->
        TritonTensor { tensorName = inputTensorName
                     , datatype = FP32
                     , shape = shape
                     , data_ = concat $ toList $ lowerElements <$> (elements)
                     }
      _ -> error "TODO: should have passed a grace Tensor value"

    reifyTritonTensor :: TritonTensor -> GraceValue.Value
    reifyTritonTensor TritonTensor { data_, shape } =
      GraceValue.Tensor (Monotype.TensorShape shape) $ fromList (fmap reifyElement data_)

  let inputs = case value of
        tensor@(GraceValue.Tensor _ _) -> [lowerTensorValue tensor]
        _ -> error "TODO: Unimplemented: input value is a record with multiple tensors"
  InferenceResponse { outputs = [outputTensor] } <- infer manager mmName (InferenceRequest { inputs = inputs })

  return $ reifyTritonTensor outputTensor
      

tritonPrimitivesForModel :: ModelMetadata -> (Text, GraceType.Type Location, GraceValue.Value)
tritonPrimitivesForModel ModelMetadata { mmName, mmInputs, mmOutputs } =
  let
    location = Location
      { name = "triton"
      , code = "http"
      , offset = Offset 0
      }
    reifyTensor (TritonTensorType { tvtName, tvtShape }) = (tvtName, GraceType.Tensor
      { shape = GraceType.Shape {tensorShape = Monotype.TensorShape tvtShape, ..}
      , type_ = GraceType.Scalar { scalar = Monotype.Real, .. }
      , ..
      })

    inputRecord = Record
      { fields = GraceType.Fields (reifyTensor <$> mmInputs) Monotype.EmptyFields
      , ..
      }
    outputRecord = Record
      { fields = GraceType.Fields (reifyTensor <$> mmOutputs) Monotype.EmptyFields
      , ..
      }
    inputType = case uncons mmInputs of
      Just (tensor, []) -> snd $ reifyTensor tensor
      _ -> inputRecord
    outputType = case uncons mmOutputs of
      Just (tensor, []) -> snd $ reifyTensor tensor
      _ -> outputRecord
    type_ = GraceType.Function
      { input = inputType, output = outputType, .. }
  in
    ("triton_" <> mmName, type_, GraceValue.TritonCall ("triton_" <> mmName))


-- * Inference

infer :: HTTP.Manager -> Text -> InferenceRequest -> IO InferenceResponse
infer manager modelName inferenceRequest = do
  let url = "http://localhost:8000/v2/models/" <> modelName <> "/infer"
  res <- HTTP.fetchWithBody manager url (decodeUtf8 $ toStrict $ encode inferenceRequest)
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

data DataType
  = FP32
  | INT64
  deriving (Eq, Show)

instance ToJSON DataType where
  toJSON d = case d of
    FP32 -> toJSON ("FP32" :: String)
    INT64 -> toJSON ("INT64" :: String)

instance FromJSON DataType where
  parseJSON = withText  "DataType"$ \(txt :: Text) -> case txt of
    "FP32" -> pure FP32
    "INT64" -> pure INT64
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
  , mmInputs :: [TritonTensorType]
  , mmOutputs :: [TritonTensorType]
  }
  deriving (Eq, Show)

instance FromJSON ModelMetadata where
  parseJSON = withObject "ModelMetadata" $ \o -> do
    mmName <- o .: "name"
    mmInputs <- o .: "inputs"
    mmOutputs <- o .: "outputs"
    return ModelMetadata {..}

data TritonTensorType = TritonTensorType
  { tvtName :: Text
  , tvtDatatype :: DataType
  , tvtShape :: [Int]
  }
  deriving (Eq, Show)

instance FromJSON TritonTensorType where
  parseJSON = withObject "TritonTensorType" $ \o -> do
    tvtName <- o .: "name"
    tvtDatatype <- o .: "datatype"
    tvtShape <- o .: "shape"
    return TritonTensorType {..}
  
listModels :: Manager -> IO [ModelMetadata]
listModels manager = do
  resp <- HTTP.fetchWithBody manager "http://localhost:8000/v2/repository/index" ""
  let (Right (repositoryResponse :: [RepositoryModel])) = eitherDecode (fromStrict $ encodeUtf8 resp)
  let modelNames = modelName <$> repositoryResponse
  forM modelNames $ \modelName -> do
    modelResp <- HTTP.fetch manager ("http://localhost:8000/v2/models/" <> modelName)
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
  models <- listModels manager
  _ <- infer manager "mnist" req

  let (_, ty, _) = tritonPrimitivesForModel (models !! 0)
  putStrLn (unpack $ Pretty.renderStrict True 60 ty)
