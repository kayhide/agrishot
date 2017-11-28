module Aws.Dynamo.Query where

import Prelude

import Aws.Dynamo.Expression (class ToOperand, Expr(..), encodeExpr, toOperand)
import Control.Monad.State (State, execState, get, modify, put)
import Data.Array ((:))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Encode, encode)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))


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


newtype Builder a = Builder (State Params a)

instance fanctorBuilder :: Functor Builder where
  map f (Builder m) = Builder $ map f m

instance applyBuilder :: Apply Builder where
  apply (Builder m) (Builder n) = Builder $ apply m n

instance applicativeBuilder :: Applicative Builder where
  pure x = Builder $ pure x

instance bindBuilder :: Bind Builder where
  bind (Builder m) f = Builder $ m >>= \x -> case f x of Builder m_ -> m_

instance monadBuilder :: Monad Builder

instance encodeBuilder :: Encode (Builder Unit) where
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


addOption :: Option -> Params -> Params
addOption opt (Params x) = Params $ x { options = opt : x.options }

tableName :: String -> Builder Unit
tableName = Builder <<< modify <<< addOption <<< TableName

indexName :: String -> Builder Unit
indexName = Builder <<< modify <<< addOption <<< IndexName

ascending :: Builder Unit
ascending = Builder <<< modify <<< addOption <<< ScanIndexForward $ true

descending :: Builder Unit
descending = Builder <<< modify <<< addOption <<< ScanIndexForward $ false

limit :: Int -> Builder Unit
limit = Builder <<< modify <<< addOption <<< Limit


keyCondition :: Expr -> Builder Unit
keyCondition expr = Builder do
  Params x <- get
  put $ Params $ x { keyCondition = expr }

filter :: Expr -> Builder Unit
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
