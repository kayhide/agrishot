module Api.Pests where

import Prelude

import Api (Client(..))
import Api as Api
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo.Query as DQ
import Control.Monad.Aff (Aff)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.StrMap as StrMap
import Model.Pest (Pest(..))


type PrimaryKey = TableKey

newtype TableKey =
  TableKey
  { id :: String
  }

derive instance genericTableKey :: Generic TableKey _
derive instance eqTableKey :: Eq TableKey

instance encodeTableKey :: Encode TableKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeTableKey :: Decode TableKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance tablekeyTableKey :: DQ.TableKey TableKey

type QueryBuilder = DQ.Builder TableKey Unit
type QueryResult = Api.QueryResult Pest TableKey


base :: Client -> QueryBuilder
base (Client cli) = do
  DQ.tableName cli.pests.tableName

listFirst :: forall eff. Client -> QueryBuilder -> Aff (dynamo :: DYNAMO | eff) QueryResult
listFirst cli q = Api.scanFirst $ base cli *> q

listFirst' :: forall eff. Client -> Aff (dynamo :: DYNAMO | eff) QueryResult
listFirst' cli = listFirst cli $ pure unit

listNext :: forall eff. Client -> TableKey -> QueryBuilder -> Aff (dynamo :: DYNAMO | eff) QueryResult
listNext cli key q = Api.scanNext key $ base cli *> q

listNext' :: forall eff. Client -> TableKey -> Aff (dynamo :: DYNAMO | eff) QueryResult
listNext' cli key = listNext cli key $ pure unit


find :: forall eff. Client -> String -> Aff (dynamo :: DYNAMO | eff) Pest
find (Client cli) id = Api.find cli.pests.tableName $ StrMap.singleton "id" id

create :: forall eff. Client -> Pest -> Aff (dynamo :: DYNAMO | eff) Unit
create = update

update :: forall eff. Client -> Pest -> Aff (dynamo :: DYNAMO | eff) Unit
update (Client cli) pest = Api.update cli.pests.tableName pest

destroy :: forall eff. Client -> Pest -> Aff (dynamo :: DYNAMO | eff) Unit
destroy (Client cli) (Pest { id }) =
  Api.destroy cli.pests.tableName $ StrMap.singleton "id" id

count :: forall eff. Client -> Aff (dynamo :: DYNAMO | eff) Int
count (Client cli) = Api.count cli.pests.tableName
