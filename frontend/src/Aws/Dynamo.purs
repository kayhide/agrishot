module Aws.Dynamo where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (kind Effect, Eff)


foreign import data DYNAMO :: Effect

foreign import _scan :: forall eff a. AwsConfig -> ScanOptions -> ((ScanResult a) -> Eff eff Unit) -> Eff eff Unit

scan :: forall eff a. AwsConfig -> ScanOptions -> Aff (dynamo :: DYNAMO | eff) (ScanResult a)
scan conf opts = makeAff callback
  where
    callback _ onSuccess = _scan conf opts onSuccess


type AwsConfig =
  { region :: String
  , endpoint :: String
  , accessKeyId :: String
  , secretAccessKey :: String
  }

type ScanOptions =
  { "TableName" :: String
  }

type ScanResult a =
  { "Items" :: Array a
  , "Count" :: Int
  , "ScannedCount" :: Int
  }
