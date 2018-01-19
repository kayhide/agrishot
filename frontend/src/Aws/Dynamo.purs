module Aws.Dynamo where

import Prelude

import Aws.Config (AwsConfig)
import Aws.Dynamo.Query as Query
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (kind Effect, Eff)
import Data.Foreign (Foreign)
import Data.Foreign.Class (encode)
import Data.Maybe (Maybe)


foreign import data DYNAMO :: Effect

foreign import setup :: forall eff. AwsConfig -> Eff (dynamo :: DYNAMO | eff) Unit


type ScanOptions =
  { "TableName" :: String
  }

type ScanResult a =
  { "Items" :: Array a
  , "Count" :: Int
  , "ScannedCount" :: Int
  }

foreign import _scan :: forall eff a. ScanOptions -> EffFnAff (dynamo :: DYNAMO | eff) (ScanResult a)

scan :: forall eff a. ScanOptions -> Aff (dynamo :: DYNAMO | eff) (ScanResult a)
scan = fromEffFnAff <<< _scan


type QueryResult a =
  { "Items" :: Array a
  , "Count" :: Int
  , "ScannedCount" :: Int
  }

foreign import _query :: forall eff a. Foreign -> EffFnAff (dynamo :: DYNAMO | eff) (QueryResult a)

query :: forall eff a. Query.Builder Unit -> Aff (dynamo :: DYNAMO | eff) (QueryResult a)
query = fromEffFnAff <<< _query <<< encode


type PutParams =
  { "TableName" :: String
  , "Item" :: Foreign
  }

foreign import _put :: forall eff. PutParams -> EffFnAff (dynamo :: DYNAMO | eff) Unit

put :: forall eff. PutParams -> Aff (dynamo :: DYNAMO | eff) Unit
put = fromEffFnAff <<< _put


type DeleteParams =
  { "TableName" :: String
  , "Key" :: Foreign
  }

foreign import _delete :: forall eff. DeleteParams -> EffFnAff (dynamo :: DYNAMO | eff) Unit

delete :: forall eff. DeleteParams -> Aff (dynamo :: DYNAMO | eff) Unit
delete = fromEffFnAff <<< _delete
