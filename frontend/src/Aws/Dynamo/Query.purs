module Aws.Dynamo.Query where

import Prelude

import Aws.Dynamo.Expression (class ToOperand, Expr(..), encodeExpr, toOperand)
import Control.Monad.State (State, execState, get, modify, put)
import Data.Array ((:))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, encode)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))


class (Eq t, Encode t, Decode t) <= TableKey t

data Option t
  = TableName String
  | IndexName String
  | ScanIndexForward Boolean
  | Limit Int
  | ExclusiveStartKey t

derive instance eqOption :: Eq t => Eq (Option t)

newtype Params t =
  Params
  { options :: Array (Option t)
  , keyCondition :: Expr
  , filter :: Expr
  }


newtype Builder t a = Builder (State (Params t) a)

instance functorBuilder :: Functor (Builder t) where
  map f (Builder m) = Builder $ map f m

instance applyBuilder :: Apply (Builder t) where
  apply (Builder m) (Builder n) = Builder $ apply m n

instance applicativeBuilder :: Applicative (Builder t) where
  pure x = Builder $ pure x

instance bindBuilder :: Bind (Builder t) where
  bind (Builder m) f = Builder $ m >>= \x -> case f x of Builder m_ -> m_

instance monadBuilder :: Monad (Builder t)

instance encodeBuilder :: TableKey t => Encode (Builder t Unit) where
  encode (Builder m) = encode $ StrMap.union (encodeOptions x.options) (encodeExpr x.keyCondition x.filter)
    where
      Params x =
        execState m $ Params { options: [], keyCondition: Blank, filter: Blank }


encodeOptions :: forall t. Encode t => Array (Option t) -> StrMap Foreign
encodeOptions opts = StrMap.fromFoldable $ encode_ <$> opts
  where
    encode_ (TableName v) = Tuple "TableName" $ encode v
    encode_ (IndexName v) = Tuple "IndexName" $ encode v
    encode_ (ScanIndexForward v) = Tuple "ScanIndexForward" $ encode v
    encode_ (Limit v) = Tuple "Limit" $ encode v
    encode_ (ExclusiveStartKey v) = Tuple "ExclusiveStartKey" $ encode v


addOption :: forall t. Option t -> Params t -> Params t
addOption opt (Params x) = Params $ x { options = opt : x.options }

tableName :: forall t. String -> Builder t Unit
tableName = Builder <<< modify <<< addOption <<< TableName

indexName :: forall t. String -> Builder t Unit
indexName = Builder <<< modify <<< addOption <<< IndexName

ascending :: forall t. Builder t Unit
ascending = Builder <<< modify <<< addOption <<< ScanIndexForward $ true

descending :: forall t. Builder t Unit
descending = Builder <<< modify <<< addOption <<< ScanIndexForward $ false

limit :: forall t. Int -> Builder t Unit
limit = Builder <<< modify <<< addOption <<< Limit

exclusiveStartKey :: forall t. t -> Builder t Unit
exclusiveStartKey = Builder <<< modify <<< addOption <<< ExclusiveStartKey


keyCondition :: forall t. Expr -> Builder t Unit
keyCondition expr = Builder do
  Params x <- get
  put $ Params $ x { keyCondition = expr }

filter :: forall t. Expr -> Builder t Unit
filter expr = Builder do
  Params x <- get
  put $ Params $ x { filter = expr }


eq_ :: forall a b. ToOperand a => ToOperand b => a -> b -> Expr
eq_ x y = EQ_ (toOperand x) (toOperand y)

ne_ :: forall a b. ToOperand a => ToOperand b => a -> b -> Expr
ne_ x y = NE_ (toOperand x) (toOperand y)

lt_ :: forall a b. ToOperand a => ToOperand b => a -> b -> Expr
lt_ x y = LT_ (toOperand x) (toOperand y)

le_ :: forall a b. ToOperand a => ToOperand b => a -> b -> Expr
le_ x y = LE_ (toOperand x) (toOperand y)

gt_ :: forall a b. ToOperand a => ToOperand b => a -> b -> Expr
gt_ x y = GT_ (toOperand x) (toOperand y)

ge_ :: forall a b. ToOperand a => ToOperand b => a -> b -> Expr
ge_ x y = GE_ (toOperand x) (toOperand y)

between_ :: forall a b. ToOperand a => ToOperand b => a -> b -> b -> Expr
between_ x y z = BETWEEN_ (toOperand x) (toOperand y) (toOperand z)

in_ :: forall a b. ToOperand a => ToOperand b => a -> Array b -> Expr
in_ x ys = IN_ (toOperand x) (toOperand <$> ys)

and_ :: Array Expr -> Expr
and_ = AND_

or_ :: Array Expr -> Expr
or_ = OR_

not_ :: Expr -> Expr
not_ = NOT_
