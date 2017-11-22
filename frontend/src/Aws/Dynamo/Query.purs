module Aws.Dynamo.Query where

import Prelude

import Control.Monad.State (State, execState, modify)
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
  }

type ParamsState = State Params Unit

newtype Builder = Builder ParamsState

instance encodeBuilder :: Encode Builder where
  encode (Builder m) = encode $ encodeOptions options
    where
      Params { options } = execState m $ Params { options: [] }

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
