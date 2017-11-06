module Aws.Dynamo where

import Prelude

import Aws.Config (AwsConfig)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (kind Effect, Eff)


foreign import data DYNAMO :: Effect

foreign import setup :: forall c eff. AwsConfig c -> Eff (dynamo :: DYNAMO | eff) Unit

foreign import _scan :: forall c eff a. AwsConfig c -> ScanOptions -> ((ScanResult a) -> Eff eff Unit) -> Eff eff Unit

scan :: forall c eff a. AwsConfig c -> ScanOptions -> Aff (dynamo :: DYNAMO | eff) (ScanResult a)
scan conf opts = makeAff callback
  where
    callback _ onSuccess = _scan conf opts onSuccess


type ScanOptions =
  { "TableName" :: String
  }

type ScanResult a =
  { "Items" :: Array a
  , "Count" :: Int
  , "ScannedCount" :: Int
  }
