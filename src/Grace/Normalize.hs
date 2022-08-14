{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module contains the logic for efficiently evaluating an expression
module Grace.Normalize
    ( -- * Normalization
      evaluate
    , quote
    , apply
    ) where

import Data.Scientific (Scientific)
import qualified Data.Foldable as Foldable
import Data.Sequence (Seq(..), ViewL(..))
import Data.Text (Text)
import Data.Void (Void)
import Data.Foldable (toList)
import Debug.Trace (trace, traceShowId)
import Grace.Location (Location)
import Grace.Infer (typeOf)
import Grace.Image (Img(..), imageToTensor, imageFromTensor)
import Grace.Syntax (Builtin(..), Scalar(..), Syntax)
import Grace.Type (Type(..))
import qualified Grace.Triton as Triton
import Grace.Monotype (TensorShape(..))
import Grace.Pretty (renderStrict)
import Grace.Value (Closure(..), Value)
import Prelude hiding (succ)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Grace.Syntax as Syntax
import qualified Grace.Value as Value

import Data.JSString (JSString)
import qualified Data.JSString as JSString
import qualified Data.JSString.Text as JSString

{- $setup

   >>> :set -XOverloadedStrings
-}

{-| Lookup a variable from an ordered environment of name-value pairs using the
    variable's name and index
-}
lookupVariable
    :: Text
    -- ^ Variable name
    -> Int
    -- ^ Variable index
    -> [(Text, Value)]
    -- ^ Evaluation environment
    -> Value
lookupVariable name index environment =
    case environment of
        (key, value) : rest ->
            if name == key
            then if index == 0
                 then value
                 else lookupVariable name (index - 1) rest
            else lookupVariable name index rest
        [] ->
            -- In the `Value` type, free variables are stored using negative
            -- indices (starting at -1) to avoid collision with bound variables
            --
            -- >>> evaluate [] "x"
            -- Variable "x" (-1)
            --
            -- This has the nice property that `quote` does the right thing when
            -- converting back to the `Syntax` type.
            Value.Variable name (negate index - 1)

{-| Substitute an expression into a `Closure`

    > instantiate (Closure name env expression) value =
    >    evaluate ((name, value) : env) expression
-}
instantiate :: Closure -> Value -> Value
instantiate (Closure name env syntax) value =
    evaluate Nothing ((name, value) : env) syntax

asInteger :: Scalar -> Maybe Integer
asInteger (Natural n) = Just (fromIntegral n)
asInteger (Integer n) = Just n
asInteger  _          = Nothing

asReal :: Scalar -> Maybe Scientific
asReal (Natural n) = Just (fromIntegral n)
asReal (Integer n) = Just (fromInteger  n)
asReal (Real    n) = Just n
asReal  _          = Nothing

{-| Evaluate an expression, leaving behind a `Value` free of reducible
    sub-expressions

    This function uses separate types for the input (i.e. `Syntax`) and the
    output (i.e. `Value`) in order to avoid wastefully evaluating the same
    sub-expression multiple times.
-}
evaluate
    :: Maybe (Type Location)
    -> [(Text, Value)]
    -- ^ Evaluation environment (starting at @[]@ for a top-level expression)
    -> Syntax Location (Type Location, Value)
    -- ^ Surface syntax
    -> Value
    -- ^ Result, free of reducible sub-expressions
evaluate type_ env syntax =
    case syntax of
        Syntax.Variable{..} ->            lookupVariable name index env

        Syntax.Application{..} -> apply function' argument' (appType)
          where
            function' = evaluate type_ env function
            argument' = evaluate type_ env argument
            Right appType = typeOf syntax

        Syntax.Lambda{..} ->
            Value.Lambda (Closure name env body)

        Syntax.Annotation{..} ->
            evaluate (Just annotation) env annotated

        Syntax.Let{..} ->
            evaluate type_ (foldl snoc env bindings) body
          where
            snoc environment Syntax.Binding{ name, assignment} =
                (name, evaluate type_ environment assignment) : environment

        Syntax.List{..} ->
            Value.List (fmap (evaluate type_ env) elements)

        Syntax.Record{..} ->
            Value.Record (HashMap.fromList (map adapt fieldValues))
          where
            adapt (key, value) = (key, evaluate type_ env value)

        Syntax.Field{..} ->
            case evaluate type_ env record of
                Value.Record fieldValues
                    | Just value <- HashMap.lookup field fieldValues ->
                        value
                other ->
                    Value.Field other field

        Syntax.Alternative{..} ->
            Value.Alternative name

        Syntax.Merge{..} ->
            Value.Merge (evaluate type_ env handlers)

        Syntax.If{..} ->
            case predicate' of
                Value.Scalar (Bool True) -> ifTrue'
                Value.Scalar (Bool False) -> ifFalse'
                _ -> Value.If predicate' ifTrue' ifFalse'
          where
            predicate' = evaluate type_ env predicate
            ifTrue'    = evaluate type_ env ifTrue
            ifFalse'   = evaluate type_ env ifFalse

        Syntax.Scalar{..} ->
            Value.Scalar scalar

        Syntax.Operator{ operator = Syntax.And, .. } ->
            case left' of
                Value.Scalar (Bool True) -> right'
                Value.Scalar (Bool False) -> Value.Scalar (Bool False)
                _ -> case right' of
                    Value.Scalar (Bool True) -> left'
                    Value.Scalar (Bool False) -> Value.Scalar (Bool False)
                    _ -> Value.Operator left' Syntax.And right'
          where
            left'  = evaluate type_ env left
            right' = evaluate type_ env right

        Syntax.Operator{ operator = Syntax.Or, .. } ->
            case left' of
                Value.Scalar (Bool True) -> Value.Scalar (Bool True)
                Value.Scalar (Bool False) -> right'
                _ -> case right' of
                    Value.Scalar (Bool True) -> Value.Scalar (Bool True)
                    Value.Scalar (Bool False) -> left'
                    _ -> Value.Operator left' Syntax.Or right'
          where
            left'  = evaluate type_ env left
            right' = evaluate type_ env right

        Syntax.Operator{ operator = Syntax.Times, .. } ->
            case (left', right') of
                (Value.Scalar (Natural 1), _) ->
                    right'
                (Value.Scalar (Natural 0), _) ->
                    Value.Scalar (Natural 0)
                (_, Value.Scalar (Natural 1)) ->
                    left'
                (_, Value.Scalar (Natural 0)) ->
                    Value.Scalar (Natural 0)
                (Value.Scalar l, Value.Scalar r)
                    | Natural m <- l
                    , Natural n <- r ->
                        Value.Scalar (Natural (m * n))
                    | Just m <- asInteger l
                    , Just n <- asInteger r ->
                        Value.Scalar (Integer (m * n))
                    | Just m <- asReal l
                    , Just n <- asReal r ->
                        Value.Scalar (Real (m * n))
                _ ->
                    Value.Operator left' Syntax.Times right'
          where
            left'  = evaluate type_ env left
            right' = evaluate type_ env right

        Syntax.Operator{ operator = Syntax.Plus, .. } ->
            case (left', right') of
                (Value.Scalar (Natural 0), _) ->
                    right'
                (_, Value.Scalar (Natural 0)) ->
                    left'
                (Value.Scalar (Text ""), _) ->
                    right'
                (_, Value.Scalar (Text "")) ->
                    left'
                (Value.List [], _) ->
                    right'
                (_, Value.List []) ->
                    left'
                (Value.Scalar l, Value.Scalar r)
                    | Natural m <- l
                    , Natural n <- r ->
                        Value.Scalar (Natural (m + n))
                    | Just m <- asInteger l
                    , Just n <- asInteger r ->
                        Value.Scalar (Integer (m + n))
                    | Just m <- asReal l
                    , Just n <- asReal r ->
                        Value.Scalar (Real (m + n))
                    | Text m <- l
                    , Text n <- r ->
                        Value.Scalar (Text (m <> n))
                (Value.List l, Value.List r) ->
                    Value.List (l <> r)
                _ ->
                    Value.Operator left' Syntax.Plus right'
          where
            left'  = evaluate type_ env left
            right' = evaluate type_ env right

        Syntax.Builtin{..} ->
            Value.Builtin builtin

        Syntax.Embed{ embedded = (_, value) } ->
            value

        Syntax.Tensor{..} ->
          let
            normalizeElements xs = concatMap normalizeElement xs
            normalizeElement x = case x of
              Syntax.Scalar {} -> [evaluate type_ env x]
              Syntax.List { elements = xs } -> normalizeElements xs
          in
            Value.Tensor shape (Seq.fromList $ normalizeElements elements)

        Syntax.TritonCall{..} -> dbg "Normalize triton call" $ Value.TritonCall modelName

{-| This is the function that implements function application, including
    evaluating anonymous functions and evaluating all built-in functions.
-}
apply :: Value -> Value -> Type Location -> Value
apply (Value.Lambda (Closure name capturedEnv body)) argument _ =
    evaluate Nothing ((name, argument) : capturedEnv) body
apply
    (Value.Merge (Value.Record alternativeHandlers))
    (Value.Application (Value.Alternative alternative) x) type_
    | Just f <- HashMap.lookup alternative alternativeHandlers =
        apply f x type_ -- TOOD I'm not sure if type_ is correct here.
apply
    (Value.Application (Value.Builtin ListDrop) (Value.Scalar (Natural n)))
    (Value.List elements) _ =
        Value.List (Seq.drop (fromIntegral n) elements)
apply
    (Value.Application (Value.Builtin ListTake) (Value.Scalar (Natural n)))
    (Value.List elements) _ =
        Value.List (Seq.take (fromIntegral n) elements)
apply (Value.Builtin ListHead) (Value.List []) _ =
    Value.Application (Value.Alternative "None") (Value.Record [])
apply (Value.Builtin ListHead) (Value.List (x :<| _)) _ =
    Value.Application (Value.Alternative "Some") x
apply (Value.Builtin ListLast) (Value.List []) _ =
    Value.Application (Value.Alternative "None") (Value.Record [])
apply (Value.Builtin ListLast) (Value.List (_ :|> x)) _ =
    Value.Application (Value.Alternative "Some") x
apply (Value.Builtin ListReverse) (Value.List xs) _ =
    Value.List (Seq.reverse xs)
apply ((Value.Application (Value.Builtin ListTopNLabels) (Value.Scalar (Natural n)))) (Value.List elems) type_ =
    let
      get_value (Value.Record fields) = case HashMap.lookup "value" fields of
        Just (Value.Scalar (Real v)) -> v
        Just (Value.Scalar (Natural v)) -> realToFrac v
        e -> error ("Expected Scalar Real, found " <> show e)
      get_value e = error ("get_value called on invalid value " <> show e)
      sorted_elems = List.sortBy (\elemA  elemB -> compare (get_value elemB) (get_value elemA)) $ toList elems
    in Value.List (Seq.fromList (List.take (fromIntegral $ toInteger n) sorted_elems))
apply (Value.Application (Value.Application (Value.Builtin ListZipWith) f) (Value.List elemsA)) (Value.List elemsB) type_ =
    Value.List (Seq.zipWith (\a b -> apply (apply f a type_) b type_) elemsA elemsB)
apply (Value.TritonCall modelName) tensor@(Value.Tensor _ _) _ =
    unsafePerformIO $ Triton.normalizeTritonCallApplication modelName tensor
apply (Value.TritonCall modelName) tensors@(Value.Record _) _ =
    unsafePerformIO $ Triton.normalizeTritonCallApplication modelName tensors
apply
    (Value.Application
        (Value.Application (Value.Builtin ListEqual) f)
        (Value.List rs)
    )
    (Value.List ls) type_
        | length ls /= length rs =
            Value.Scalar (Bool False)
        | Just bools <- traverse toBool (Seq.zipWith equal ls rs) =
        Value.Scalar (Bool (and bools))
      where
        toBool (Value.Scalar (Bool b)) = Just b
        toBool  _                      = Nothing

        equal l r = apply (apply f l type_) r type_ -- TODO: I'm not sure if type_ is correct here.
apply
    (Value.Application
        (Value.Builtin ListFold)
        (Value.Record
            (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                [ ("cons"  , cons)
                , ("nil"   , nil)
                ]
            )
        )
    )
    (Value.List elements) type_ = loop (Seq.reverse elements) nil
  where
    loop xs !result =
        case Seq.viewl xs of
            EmptyL  -> result
            y :< ys -> loop ys (apply (apply cons y type_) result type_) -- TODO: I'm not sure if type_ is right here.
apply (Value.Builtin (ImageToTensor (TensorShape shape))) (Value.Scalar (Syntax.Image imageBytes)) resultType =
  dbg "NORMALIZE Image/toTensor" $
  case imageToTensor (Img imageBytes) shape of
    Left err -> error err
    Right ((width, height), elements) ->
      let
        tensorElements = (\v -> Value.Scalar (Syntax.Real $ realToFrac v)) <$> elements
        newShape = case shape of
          [-1,-1,-1,3] -> [1,height, width, 3]
          [1,3,_,_] -> [1,3, height, width]
          _ -> error $ "Don't know what to do to normalize this shape: " <> show shape
      in Value.Tensor (trace (show newShape) $ TensorShape newShape) (Seq.fromList tensorElements)
apply (Value.Builtin (ImageFromTensor (TensorShape shape))) x@(Value.Tensor (TensorShape runtimeShape) elements) resultType =
  dbg "NORMALIZE Image/fromTensor" $
  let
    floatElements = (\(Value.Scalar (Syntax.Real v)) -> realToFrac v) <$> elements
    Img img = imageFromTensor runtimeShape (Foldable.toList floatElements)
  in
    Value.Scalar (Syntax.Image img)
apply (Value.Builtin ListIndexed) (Value.List elements) _ =
    Value.List (Seq.mapWithIndex adapt elements)
  where
    adapt index value =
        Value.Record
            [ ("index", Value.Scalar (Natural (fromIntegral index)))
            , ("value", value)
            ]
apply (Value.Builtin ListLength) (Value.List elements) _ =
    Value.Scalar (Natural (fromIntegral (length elements)))
apply
    (Value.Application (Value.Builtin ListMap) f)
    (Value.List elements) type_ =
        Value.List (fmap (\a -> apply f a type_) elements) -- TODO: I don't think type_ is correct here.
apply
    (Value.Application
        (Value.Application
            (Value.Builtin NaturalFold)
            (Value.Scalar (Natural n))
        )
        succ
    )
    zero type_ =
        go n zero
  where
    go 0 !result = result
    go m !result = go (m - 1) (apply succ result type_) -- TODO: I don't think type_ is right here.
apply (Value.Builtin IntegerEven) (Value.Scalar x) _
    | Just n <- asInteger x = Value.Scalar (Bool (even n))
apply (Value.Builtin IntegerOdd) (Value.Scalar x) _
    | Just n <- asInteger x = Value.Scalar (Bool (odd n))
apply
    (Value.Application (Value.Builtin RealEqual) (Value.Scalar l))
    (Value.Scalar r) _
    | Just m <- asReal l
    , Just n <- asReal r =
        Value.Scalar (Bool (m == n))
apply
    (Value.Application (Value.Builtin RealLessThan) (Value.Scalar l))
    (Value.Scalar r) _
    | Just m <- asReal l
    , Just n <- asReal r =
        Value.Scalar (Bool (m < n))
apply (Value.Builtin IntegerAbs) (Value.Scalar x) _
    | Just n <- asInteger x = Value.Scalar (Natural (fromInteger (abs n)))
apply (Value.Builtin RealNegate) (Value.Scalar x) _
    | Just n <- asReal x = Value.Scalar (Real (negate n))
apply (Value.Builtin IntegerNegate) (Value.Scalar x) _
    | Just n <- asInteger x = Value.Scalar (Integer (negate n))
apply (Value.Builtin RealShow) (Value.Scalar (Natural n)) _ =
    Value.Scalar (Text (Text.pack (show n)))
apply (Value.Builtin RealShow) (Value.Scalar (Integer n)) _ =
    Value.Scalar (Text (Text.pack (show n)))
apply (Value.Builtin RealShow) (Value.Scalar (Real n)) _ =
    Value.Scalar (Text (Text.pack (show n)))
apply (Value.Builtin TensorFromList) (Value.List xs) type_ =
  case type_ of
    Tensor _ (Shape { tensorShape = TensorShape dims }) _  ->
      let
        -- TODO: What if the shape contains -1?
        expectedLength = List.foldl' (*) 1 dims
      in
        if expectedLength == length xs
        then Value.Tensor (TensorShape dims) xs
        else error "Dimension mismatch"
    _ -> error $ "Impossible case, application does not results in not a tensor: " <> Text.unpack (renderStrict True 50 type_)
apply (Value.Builtin TensorToList) (Value.Tensor _ xs) _ =
  Value.List xs
apply
    (Value.Application (Value.Builtin TextEqual) (Value.Scalar (Text l)))
    (Value.Scalar (Text r)) _ =
        Value.Scalar (Bool (l == r))
apply
    (Value.Application
        (Value.Builtin JSONFold)
        (Value.Record
            (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                [ ("array"  , arrayHandler )
                , ("bool"   , boolHandler  )
                , ("integer", integerHandler)
                , ("natural", naturalHandler)
                , ("null"   , nullHandler   )
                , ("object" , objectHandler )
                , ("real"   , realHandler  )
                , ("string" , stringHandler )
                ]
            )
        )
    )
    v0 type_ = loop v0
  where
    loop (Value.Scalar (Bool b)) =
        apply boolHandler (Value.Scalar (Bool b)) type_ -- TODO: is _type right here?
    loop (Value.Scalar (Natural n)) =
        apply naturalHandler (Value.Scalar (Natural n)) type_ -- TODO: is type_ right here?
    loop (Value.Scalar (Integer n)) =
        apply integerHandler (Value.Scalar (Integer n)) type_ -- TODO: type_?
    loop (Value.Scalar (Real n)) =
        apply realHandler (Value.Scalar (Real n)) type_ -- TODO: Type
    loop (Value.Scalar (Text t)) =
        apply stringHandler (Value.Scalar (Text t)) type_ -- TODO: Type?
    loop (Value.Scalar Null) =
        nullHandler
    loop (Value.List elements) =
        apply arrayHandler (Value.List (fmap loop elements)) type_ -- TODO: Here type_
    loop (Value.Record keyValues) =
        apply objectHandler (Value.List (Seq.fromList (map adapt (HashMap.toList keyValues)))) type_ -- TODO: type :(
      where
        adapt (key, value) =
            Value.Record
                [("key", Value.Scalar (Text key)), ("value", loop value)]
    loop v =
        v
apply function argument _ =
    Value.Application function argument

countNames :: Text -> [Text] -> Int
countNames name = length . filter (== name)

{-| Obtain a unique variable, given a list of variable names currently in scope

    >>> fresh "x" [ "x", "y", "x" ]
    Variable "x" 2
    >>> fresh "y" [ "x", "y", "x" ]
    Variable "y" 1
    >>> fresh "z" [ "x", "y", "x" ]
    Variable "z" 0
-}
fresh
    :: Text
    -- ^ Variable base name (without the index)
    -> [Text]
    -- ^ Variables currently in scope
    -> Value
    -- ^ Unique variable (including the index)
fresh name names = Value.Variable name (countNames name names)

-- | Convert a `Value` back into the surface `Syntax`
quote
    :: [Text]
    -- ^ Variable names currently in scope (starting at @[]@ for a top-level
    --   expression)
    -> Value
    -> Syntax () Void
quote names value =
    case value of
        Value.Variable name index ->
            Syntax.Variable{ index = countNames name names - index - 1, .. }

        Value.Lambda closure@(Closure name _ _) ->
            Syntax.Lambda{ nameLocation = (), .. }
          where
            variable = fresh name names

            body = quote (name : names) (instantiate closure variable)

        Value.Application function argument ->
            Syntax.Application
                { function = quote names function
                , argument = quote names argument
                , ..
                }

        Value.List elements ->
            Syntax.List{ elements = fmap (quote names) elements, .. }

        Value.Record fieldValues ->
            Syntax.Record
                { fieldValues = map adapt (HashMap.toList fieldValues)
                , ..
                }
          where
            adapt (field, value_) = (field, quote names value_)

        Value.Field record field ->
            Syntax.Field{ record = quote names record, fieldLocation = (), .. }

        Value.Alternative name ->
            Syntax.Alternative{..}

        Value.Merge handlers ->
            Syntax.Merge{ handlers = quote names handlers, .. }

        Value.If predicate ifTrue ifFalse ->
            Syntax.If
                { predicate = quote names predicate
                , ifTrue = quote names ifTrue
                , ifFalse = quote names ifFalse
                , ..
                }

        Value.Scalar scalar ->
            Syntax.Scalar{..}

        Value.Operator left operator right ->
            Syntax.Operator
                { left = quote names left
                , operatorLocation = ()
                , right = quote names right
                , ..
                }

        Value.Builtin builtin ->
            Syntax.Builtin{..}

        Value.Tensor _ elements ->
            Syntax.Application
                { function = Syntax.Builtin { builtin = Syntax.TensorFromList, .. }
                , argument = Syntax.List
                  { elements = fmap (quote names) elements, .. }
                , ..}
            -- Syntax.Tensor { elements = fmap  (quote names) elements, .. }

        Value.TritonCall name ->
          Syntax.Variable { index = 0, .. }
  where
    location = ()

foreign import javascript unsafe "console.log($1)"
  consoleLog_ :: JSString -> IO ()

consoleLog :: Text -> IO ()
consoleLog = consoleLog_ . JSString.pack . Text.unpack

dbg :: Text -> a -> a
dbg t a = seq (unsafePerformIO $ consoleLog t) a
