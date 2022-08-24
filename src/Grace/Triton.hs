-- | 

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grace.Triton where

import qualified Control.Concurrent.MVar as MVar
import System.IO.Unsafe (unsafePerformIO)
import Control.DeepSeq (NFData, force)
import qualified Control.Exception as Exception
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.Map as Map
import Data.List (uncons, find)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.ByteString.Lazy( fromStrict, toStrict )
import Data.Traversable (forM)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Sequence (fromList)
import qualified Data.Vector as Vector
import Data.Scientific (Scientific)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text, unpack)
import Data.Aeson (eitherDecode, withObject, (.:), encode, object, FromJSON(..), ToJSON(..), (.=), withText)
import Debug.Trace (trace)
import GHC.Generics
import Grace.HTTP as HTTP
import Grace.Type (Type(..))
import qualified Grace.Pretty as Pretty
import qualified Grace.Type as GraceType
import qualified Grace.Syntax as GraceSyntax
import qualified Grace.Value as GraceValue
import Grace.Location (Location(..), Offset(..))
import qualified Grace.Monotype as Monotype
import System.Environment (lookupEnv)

#ifdef ghcjs_HOST_OS
import qualified Data.JSString as JSString
#endif

-- * Triton Grace primitives

loadContext :: IO ( [(Text, GraceType.Type Location, GraceValue.Value)])
loadContext = do
  manager <- HTTP.newManager
  cache <- MVar.newMVar Map.empty
  models <- listModels manager (Just cache)
  return $ tritonPrimitivesForModel <$> models

time :: Text -> IO a -> IO a
time prefix action = do
  consoleLog ("about to " <> prefix)
  t0 <- getCurrentTime
  result <- action
  t1 <- getCurrentTime
  consoleLog $ prefix <> " took " <> Text.pack (show (diffUTCTime t1 t0))
  return result

normalizeTritonCallApplication :: Text -> GraceValue.Value -> Maybe (MVar.MVar FetchCache) -> IO GraceValue.Value
normalizeTritonCallApplication prefixedModelName lazyValue cache = do
  putStrLn "new http manager"
  manager <- time "HTTP.newManager" HTTP.newManager

  -- We have to look up models from scratch because single-input and single-output
  -- values drop the name of the tensor.
  putStrLn "list models"
  models <- time "list models" $ listModels manager cache
  
  putStrLn "find matching model"
  
  -- context <- loadContext -- TODO: It's pretty inefficient to do this every time we normalize an triton call.
  let

    -- This also only works for single-input, single-output models.
    Just model = find (\ModelMetadata {mmName = name} -> "triton_" <> name ==  prefixedModelName) models
    -- let inputTensorNamesAndShapes = mmInputs model

  value <- time "force the value" $ Exception.evaluate $ force lazyValue

  putStrLn "compute inputs"
  inputs <- time "compute inputs (with force)" $ Exception.evaluate $ force $ case value of
        tensor@(GraceValue.Tensor _ _) ->
          let [TritonTensorType {tvtName = inputTensorName}] = mmInputs model
          in [lowerTensorValue inputTensorName tensor]
        (GraceValue.Record fields) ->
          if length fields /= length (mmInputs model)
          then
            error "Internal error: Argument has different number of record fields than triton method expects."
          else
            fmap (\(TritonTensorType {tvtName}) ->
                    case InsOrdHashMap.lookup tvtName fields of
                      Nothing -> error $ "Internal error: no field " <> Text.unpack tvtName <> " in argument"
                      Just t  -> lowerTensorValue tvtName t
                    ) (mmInputs model)
        _ -> error "TODO: Unimplemented: input value is a record with multiple tensors"

  putStrLn "call infer"
  InferenceResponse { outputs = [outputTensor] } <- time "call infer" $ infer manager (mmName model) (InferenceRequest { inputs = inputs }) cache

  putStrLn "return from normalizeTritonCallApplication"
  return $ reifyTritonTensor outputTensor

-- | Helper functions for normalizaTritonCallApplication
reifyFloatElement :: Scientific -> GraceValue.Value
reifyFloatElement v = GraceValue.Scalar (GraceSyntax.Real $ realToFrac v)

reifyIntElement :: Scientific -> GraceValue.Value
reifyIntElement v = GraceValue.Scalar (GraceSyntax.Integer (round v))

-- | Helper functions for normalizaTritonCallApplication
lowerTensorValue :: Text -> GraceValue.Value -> TritonTensor
lowerTensorValue !inputTensorName !t =
  unsafePerformIO $ time ("lowerTensorValue " <> inputTensorName) $ Exception.evaluate $ force $
    case t of
      GraceValue.Tensor (Monotype.TensorShape shape) elements -> do
        (datatype, xs) <-
              case elements of
                GraceValue.TensorIntElements ints -> pure (INT64, undefined)
                GraceValue.TensorFloatElements floats -> pure (FP32, fmap realToFrac (Vector.toList floats))
        pure $ TritonTensor { tensorName = inputTensorName
                      , datatype
                      , shape = shape
                      , data_ = xs -- = concat $ toList $ lowerElements <$> (elements)
                      }
      _ -> error "TODO: should have passed a grace Tensor value"

-- | Helper functions for normalizaTritonCallApplication
reifyTritonTensor :: TritonTensor -> GraceValue.Value
reifyTritonTensor TritonTensor { data_, datatype, shape } =
  case datatype of
    FP32 -> GraceValue.Tensor (Monotype.TensorShape shape) $ GraceValue.TensorFloatElements $ Vector.fromList (realToFrac <$> data_)
    INT64 -> GraceValue.Tensor (Monotype.TensorShape shape) $ GraceValue.TensorIntElements $ Vector.fromList (round <$> data_)


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

infer :: HTTP.Manager -> Text -> InferenceRequest -> Maybe (MVar.MVar FetchCache) -> IO InferenceResponse
infer manager modelName inferenceRequest cache = do
  reqData <- time "INFER: force req data" $ Exception.evaluate $ force inferenceRequest
  reqJson <- time "INFER: aeson encode data" $ Exception.evaluate $ force $ encode reqData
  reqBody <- time "INFER: utf8 process" $ Exception.evaluate (decodeUtf8 $ toStrict $ reqJson)
  putStrLn "infer: compute url"
  url <- withTritonBaseUrl $ \baseUrl -> pure $  baseUrl <> "/v2/models/" <> modelName <> "/infer"
  -- putStrLn "infer: about to fetch with this request:"
  -- print inferenceRequest
  putStrLn "infer: fetchWithBody"
  putStrLn "infer: fetchWithBody"
  res <- time "INFER: HTTP.fetchWithBody" $ HTTP.fetchWithBody manager url reqBody cache
  -- putStrLn "infer: finished fetch with this response:"
  -- print res
  putStrLn ("infer: decode response")
  resp <- time "infer: decode response" $ Exception.evaluate $ eitherDecode $ fromStrict $ encodeUtf8 res
  case resp of
    Right inferenceResponse -> do
      consoleLog "infer: Got good response"
      return inferenceResponse
    Left err -> do
      consoleLog ("infer: Got bad response: " <> Text.pack (show err))
      error "No progress"

data InferenceRequest = InferenceRequest
  { inputs :: [TritonTensor] }
  deriving (Eq, Show, Generic)

instance NFData InferenceRequest

data InferenceResponse = InferenceResponse
  { outputs :: [TritonTensor] }
  deriving (Eq, Show, Generic)

instance ToJSON InferenceRequest where
instance FromJSON InferenceResponse where

data TritonTensor = TritonTensor
  { tensorName :: Text
  , datatype :: DataType
  , shape :: [Int]
  , data_ :: [Scientific]
  }
  deriving (Eq, Show, Generic)

instance NFData TritonTensor

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
  deriving (Eq, Show, Generic)

instance NFData DataType

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
  
listModels :: Manager -> Maybe (MVar.MVar FetchCache) -> IO [ModelMetadata]
listModels manager cache = do
  resp <- withTritonBaseUrl $ \baseUrl -> HTTP.fetchWithBody manager (baseUrl <> "/v2/repository/index") "" cache
  let (Right (repositoryResponse :: [RepositoryModel])) = eitherDecode (fromStrict $ encodeUtf8 resp)
  let modelNames = modelName <$> repositoryResponse
  forM modelNames $ \modelName -> do
    modelResp <- withTritonBaseUrl $ \baseUrl -> HTTP.fetch manager (baseUrl <> "/v2/models/" <> modelName)
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
  models <- listModels manager Nothing
  _ <- infer manager "mnist" req Nothing

  let (_, ty, _) = tritonPrimitivesForModel (models !! 0)
  putStrLn (unpack $ Pretty.renderStrict True 60 ty)

withTritonBaseUrl :: (Text -> IO a) -> IO a
withTritonBaseUrl go = do
  envHost <- lookupEnv "TRITON_BASE_URL"
  go (Text.pack $ fromMaybe
      -- "http://100.127.86.34:8000"
      "http://localhost:8000"
      envHost)

consoleLog :: Text -> IO ()
#ifdef ghcjs_HOST_OS
consoleLog = consoleLog_ . JSString.pack . Text.unpack

foreign import javascript unsafe "console.log($1)"
  consoleLog_ :: JSString.JSString -> IO ()
#else
conseleLog = putStrLn
#endif
