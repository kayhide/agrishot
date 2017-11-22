module Aws.Dynamo.Query where

import Prelude

import Control.Monad.State (State, evalState, execState, get, gets, modify, put)
import Data.Array ((:), (..))
import Data.Array as Array
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Encode, encode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple

data Option
  = TableName String
  | IndexName String
  | ScanIndexForward Boolean
  | Limit Int

derive instance eqOption :: Eq Option

newtype Params =
  Params
  { options :: Array Option
  , keyCondition :: Expr
  , filter :: Expr
  }

type ParamsState = State Params Unit

newtype Builder = Builder ParamsState

instance encodeBuilder :: Encode Builder where
  encode (Builder m) = encode $ StrMap.union (encodeOptions x.options) (encodeExpr x.keyCondition x.filter)
    where
      Params x =
        execState m $ Params { options: [], keyCondition: Blank, filter: Blank }

encodeOptions :: Array Option -> StrMap Foreign
encodeOptions opts = StrMap.fromFoldable $ encode_ <$> opts
  where
    encode_ (TableName v) = Tuple "TableName" $ encode v
    encode_ (IndexName v) = Tuple "IndexName" $ encode v
    encode_ (ScanIndexForward v) = Tuple "ScanIndexForward" $ encode v
    encode_ (Limit v) = Tuple "Limit" $ encode v

build :: ParamsState -> Builder
build = Builder

addOption :: Option -> Params -> Params
addOption opt (Params x) = Params $ x { options = opt : x.options }

tableName :: String -> ParamsState
tableName = modify <<< addOption <<< TableName

indexName :: String -> ParamsState
indexName = modify <<< addOption <<< IndexName

ascending :: ParamsState
ascending = modify <<< addOption <<< ScanIndexForward $ true

descending :: ParamsState
descending = modify <<< addOption <<< ScanIndexForward $ false

limit :: Int -> ParamsState
limit = modify <<< addOption <<< Limit


class Encode a <= ToFactor a where
  toFactor :: a -> Factor
instance factorInt :: ToFactor Int where
  toFactor = I
instance factorNumber :: ToFactor Number where
  toFactor = N
instance factorString :: ToFactor String where
  toFactor = S
instance factorBoolean :: ToFactor Boolean where
  toFactor = B

data Factor
  = I Int
  | N Number
  | S String
  | B Boolean

data Expr
  = Blank
  | EQ_ String Factor
  | NE_ String Factor
  | LT_ String Factor
  | LE_ String Factor
  | GT_ String Factor
  | GE_ String Factor
  | BETWEEN_ String Factor Factor
  | IN_ String (Array Factor)
  | AND_ Expr Expr
  | OR_ Expr Expr

derive instance eqFactor :: Eq Factor
derive instance genericFactor :: Generic Factor _
instance showFactor :: Show Factor where
  show = genericShow

instance encodeFactor :: Encode Factor where
  encode (I v) = encode v
  encode (N v) = encode v
  encode (S v) = encode v
  encode (B v) = encode v

derive instance eqExpr :: Eq Expr
derive instance genericExpr :: Generic Expr _
instance showExpr :: Show Expr where
  show expr = genericShow expr

encodeExpr :: Expr -> Expr -> StrMap Foreign
encodeExpr keyCondition filter = StrMap.fromFoldable $ evalState interpret_ StrMap.empty
  where
    interpret_ = do
      keyCondition_ <- interpret keyCondition
      filter_ <- interpret filter
      names <- gets StrMap.keys
      pure [ Tuple "ExpressionAttributeNames" $ encode $ StrMap.fromFoldable $ (append "#" &&& encode) <$> names
           , Tuple "ExpressionAttributeValues" $ encode $ StrMap.fromFoldable $ keyCondition_.values <> filter_.values
           , Tuple "KeyConditionExpression" $ encode keyCondition_.expression
           , Tuple "FilterExpression" $ encode filter_.expression
           ]


type InterExpr =
  { values :: Array (Tuple String Foreign)
  , expression :: String
  }

type ValueCache = StrMap Int
type Operator = String
type PropertyKey = String

toPropertyValue :: PropertyKey -> Int -> Factor -> Tuple String Foreign
toPropertyValue k 0 v = Tuple (":" <> k) (encode v)
toPropertyValue k n v = Tuple (":" <> k <> "__" <> show n) (encode v)

interpretBinary :: Operator -> PropertyKey -> Factor -> State ValueCache InterExpr
interpretBinary op k v = do
  n <- fromMaybe 0 <$> gets (StrMap.lookup k)
  modify $ StrMap.insert k (n + 1)
  let v_ = toPropertyValue k n v
  pure { values: [ v_ ]
       , expression: "#" <> k <> " " <> op <> " " <> Tuple.fst v_
       }

interpretTernary :: Operator -> PropertyKey -> Factor -> Factor -> State ValueCache InterExpr
interpretTernary op k v1 v2 = do
  n <- fromMaybe 1 <$> gets (StrMap.lookup k)
  modify $ StrMap.insert k (n + 2)
  let v1_ = toPropertyValue k n v1
      v2_ = toPropertyValue k (n + 1) v2
  pure { values: [ v1_, v2_ ]
       , expression: "#" <> k <> " " <> op <> " " <> Tuple.fst v1_ <> " AND " <> Tuple.fst v2_
       }

interpretMultiary :: Operator -> PropertyKey -> Array Factor -> State ValueCache InterExpr
interpretMultiary op k vs = do
  n <- fromMaybe 1 <$> gets (StrMap.lookup k)
  let m = n + Array.length vs
  modify $ StrMap.insert k m
  let vs_ = Array.zipWith (toPropertyValue k) (n .. (m - 1)) vs
  pure { values: vs_
       , expression: "#" <> k <> " " <> op <> " (" <> (Array.intercalate "," <<< map Tuple.fst $ vs_) <> ")"
       }

interpret :: Expr -> State ValueCache InterExpr
interpret Blank = pure { values: [], expression: "" }

interpret (EQ_ k v) = interpretBinary "=" k v
interpret (NE_ k v) = interpretBinary "<>" k v
interpret (LT_ k v) = interpretBinary "<" k v
interpret (LE_ k v) = interpretBinary "<=" k v
interpret (GT_ k v) = interpretBinary ">" k v
interpret (GE_ k v) = interpretBinary ">=" k v
interpret (BETWEEN_ k v1 v2) = interpretTernary "BETWEEN" k v1 v2
interpret (IN_ k vs) = interpretMultiary "IN" k vs

interpret (AND_ e1 e2) = do
  e1_ <- interpret e1
  e2_ <- interpret e2
  pure { values: e1_.values <> e2_.values
       , expression: "(" <> e1_.expression <> ") AND (" <> e2_.expression <> ")"
       }
interpret (OR_ e1 e2) = do
  e1_ <- interpret e1
  e2_ <- interpret e2
  pure { values: e1_.values <> e2_.values
       , expression: "(" <> e1_.expression <> ") OR (" <> e2_.expression <> ")"
       }


keyCondition :: Expr -> ParamsState
keyCondition expr = do
  Params x <- get
  put $ Params $ x { keyCondition = expr }

filter :: Expr -> ParamsState
filter expr = do
  Params x <- get
  put $ Params $ x { filter = expr }
