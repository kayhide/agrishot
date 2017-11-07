module Aws.Dynamo where

import Prelude

import Aws.Config (AwsConfig)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (kind Effect, Eff)


foreign import data DYNAMO :: Effect

foreign import setup :: forall c eff. AwsConfig c -> Eff (dynamo :: DYNAMO | eff) Unit

foreign import _scan :: forall eff a. ScanOptions -> EffFnAff (dynamo :: DYNAMO | eff) (ScanResult a)

scan :: forall eff a. ScanOptions -> Aff (dynamo :: DYNAMO | eff) (ScanResult a)
scan = _scan >>> fromEffFnAff


type ScanOptions =
  { "TableName" :: String
  }

type ScanResult a =
  { "Items" :: Array a
  , "Count" :: Int
  , "ScannedCount" :: Int
  }
