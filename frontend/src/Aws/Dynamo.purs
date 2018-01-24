module Aws.Dynamo where

import Prelude

import Aws.Config (AwsConfig)
import Aws.Dynamo.Query as Query
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (kind Effect, Eff)
import Data.Foreign (Foreign)
import Data.Foreign.Class (encode)


foreign import data DYNAMO :: Effect

foreign import setup :: forall eff. AwsConfig -> Eff (dynamo :: DYNAMO | eff) Unit
foreign import verbose :: forall eff. Boolean -> Eff (dynamo :: DYNAMO | eff) Unit


foreign import _scan :: forall eff a. Foreign -> EffFnAff (dynamo :: DYNAMO | eff) Foreign

scan :: forall eff a. Query.Builder Unit -> Aff (dynamo :: DYNAMO | eff) Foreign
scan = fromEffFnAff <<< _scan <<< encode


foreign import _query :: forall eff a. Foreign -> EffFnAff (dynamo :: DYNAMO | eff) Foreign

query :: forall eff a. Query.Builder Unit -> Aff (dynamo :: DYNAMO | eff) Foreign
query = fromEffFnAff <<< _query <<< encode


type GetParams =
  { "TableName" :: String
  , "Key" :: Foreign
  }

type GetResult a =
  { "Item" :: a
  }

foreign import _get :: forall eff a. GetParams -> EffFnAff (dynamo :: DYNAMO | eff) (GetResult a)

get :: forall eff forall a. GetParams -> Aff (dynamo :: DYNAMO | eff) (GetResult a)
get = fromEffFnAff <<< _get


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


foreign import _count :: forall eff. String -> EffFnAff (dynamo :: DYNAMO | eff) Int

count :: forall eff. String -> Aff (dynamo :: DYNAMO | eff) Int
count = fromEffFnAff <<< _count
