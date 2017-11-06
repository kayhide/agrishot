module Aws.Dynamo where

import Prelude

import Aws.Config (AwsConfig)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (kind Effect, Eff)


foreign import data DYNAMO :: Effect

foreign import setup :: forall c eff. AwsConfig c -> Eff (dynamo :: DYNAMO | eff) Unit

foreign import _scan :: forall eff a. ScanOptions -> ((ScanResult a) -> Eff eff Unit) -> Eff eff Unit

scan :: forall eff a. ScanOptions -> Aff (dynamo :: DYNAMO | eff) (ScanResult a)
scan opts = makeAff callback
  where
    callback _ onSuccess = _scan opts onSuccess


type ScanOptions =
  { "TableName" :: String
  }

type ScanResult a =
  { "Items" :: Array a
  , "Count" :: Int
  , "ScannedCount" :: Int
  }
