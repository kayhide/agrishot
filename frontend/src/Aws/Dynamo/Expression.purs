module Aws.Dynamo.Expression where

import Prelude

import Control.Monad.State (State, evalState, gets, put)
import Data.Array ((:))
import Data.Array as Array
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Encode, encode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong ((&&&))
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))


class Encode a <= ToOperand a where
  toOperand :: a -> Operand
instance operandInt :: ToOperand Int where
  toOperand = I
instance operandNumber :: ToOperand Number where
  toOperand = N
instance operandBoolean :: ToOperand Boolean where
  toOperand = B
instance operandString :: ToOperand String where
  toOperand s = case String.charAt 0 s of
    Just '#' -> Path $ String.split (String.Pattern ".#") $ String.drop 1 s
    otherwise -> S s

data Operand
  = I Int
  | N Number
  | B Boolean
  | S String
  | Path (Array String)

data Expr
  = Blank
  | EQ_ Operand Operand
  | NE_ Operand Operand
  | LT_ Operand Operand
  | LE_ Operand Operand
  | GT_ Operand Operand
  | GE_ Operand Operand
  | BETWEEN_ Operand Operand Operand
  | IN_ Operand (Array Operand)
  | AND_ (Array Expr)
  | OR_ (Array Expr)
  | NOT_ Expr

derive instance eqOperand :: Eq Operand
derive instance genericOperand :: Generic Operand _
instance showOperand :: Show Operand where
  show = genericShow

instance encodeOperand :: Encode Operand where
  encode (I v) = encode v
  encode (N v) = encode v
  encode (S v) = encode v
  encode (B v) = encode v
  encode (Path v) = encode $ encode <$> v

derive instance eqExpr :: Eq Expr
derive instance genericExpr :: Generic Expr _
instance showExpr :: Show Expr where
  show expr = genericShow expr

encodeExpr :: Expr -> Expr -> StrMap Foreign
encodeExpr keyCondition filter = StrMap.fromFoldable $ evalState interpret_ 0
  where
    interpret_ = do
      keyCondition_ <- interpret keyCondition
      filter_ <- interpret filter
      let names = Array.concatMap _.names [keyCondition_, filter_]
          values = Array.concatMap _.values [keyCondition_, filter_]
          keyConditionExpression = case keyCondition of
            Blank -> []
            otherwise -> [ Tuple "KeyConditionExpression" $ encode keyCondition_.expression ]
          filterExpression = case filter of
            Blank -> []
            otherwise -> [ Tuple "FilterExpression" $ encode filter_.expression ]
          expressionAttributeNames = case names of
            [] -> []
            otherwise ->
              [ Tuple "ExpressionAttributeNames" $
                encode $ StrMap.fromFoldable $ (append "#" &&& id) <$> names
              ]
          expressionAttributeValues = case values of
            [] -> []
            otherwise ->
              [ Tuple "ExpressionAttributeValues" $
                encode $ StrMap.fromFoldable $ values
              ]
      pure $
        expressionAttributeNames <> expressionAttributeValues
        <> keyConditionExpression <> filterExpression


type InterExpr =
  { names :: Array String
  , values :: Array (Tuple String Foreign)
  , expression :: String
  }

type ValueCount = Int
type Operator = String
type Conjunction = String

interpret :: Expr -> State ValueCount InterExpr
interpret Blank = pure { names: [], values: [], expression: "" }

interpret (EQ_ v1 v2) = interpretCompare "=" v1 v2
interpret (NE_ v1 v2) = interpretCompare "<>" v1 v2
interpret (LT_ v1 v2) = interpretCompare "<" v1 v2
interpret (LE_ v1 v2) = interpretCompare "<=" v1 v2
interpret (GT_ v1 v2) = interpretCompare ">" v1 v2
interpret (GE_ v1 v2) = interpretCompare ">=" v1 v2

interpret (BETWEEN_ v1 v2 v3) = do
  v1_ <- interpretOperand v1
  v2_ <- interpretOperand v2
  v3_ <- interpretOperand v3
  pure { names: Array.concatMap _.names [v1_, v2_, v3_]
       , values: Array.concatMap _.values [v1_, v2_, v3_]
       , expression: v1_.expression <> " BETWEEN " <> v2_.expression <> " AND " <> v3_.expression
       }

interpret (IN_ v vs) = do
  v_ <- interpretOperand v
  vs_ <- traverse interpretOperand vs
  pure { names: Array.concatMap _.names (v_ : vs_)
       , values: Array.concatMap _.values (v_ : vs_)
       , expression: v_.expression <> " IN (" <> (Array.intercalate "," <<< map _.expression $ vs_) <> ")"
       }

interpret (AND_ es) = interpretConjunct "AND" es
interpret (OR_ es) = interpretConjunct "OR" es

interpret (NOT_ e) = do
  e_ <- interpret e
  pure $ e_ { expression = "NOT " <> e_.expression }


interpretOperand :: Operand -> State ValueCount InterExpr
interpretOperand (Path vs) =
  pure { names: vs
       , values: []
       , expression: Array.intercalate "." $ append "#" <$> vs
       }
interpretOperand v = do
  n <- gets (_ + 1)
  put n
  let k = ":" <> show n
  pure { names: []
       , values: [Tuple k (encode v)]
       , expression: k
       }

interpretCompare :: Operator -> Operand -> Operand -> State ValueCount InterExpr
interpretCompare op v1 v2 = do
  v1_ <- interpretOperand v1
  v2_ <- interpretOperand v2
  pure { names: Array.concatMap _.names [v1_, v2_]
       , values: Array.concatMap _.values [v1_, v2_]
       , expression: v1_.expression <> " " <> op <> " " <> v2_.expression
       }

interpretConjunct :: Conjunction -> Array Expr -> State ValueCount InterExpr
interpretConjunct conj es = do
  es_ <- traverse interpret es
  pure { names: Array.concatMap _.names es_
       , values: Array.concatMap _.values es_
       , expression: Array.intercalate (" " <> conj <> " ") <<< map (parenthesize <<< _.expression) $ es_
       }
  where
    parenthesize x = "(" <> x <> ")"
