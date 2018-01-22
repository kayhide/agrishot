module Api.Photos where

import Prelude

import Api (Client(..))
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Aws.Dynamo.Query as DQ
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (Foreign, readArray, readNullOrUndefined)
import Data.Foreign.Class (class Decode, decode, encode)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe)
import Data.StrMap as StrMap
import Data.Traversable (traverse)
import Model.Photo (Photo(..))


type TableKey =
  { id :: String
  , part :: Int
  , created_at :: Number
  }

type QueryResult a =
  { items :: Array a
  , count :: Int
  , lastKey :: Maybe TableKey
  }

listFirst :: forall eff. Client -> DQ.Builder Unit -> Aff (dynamo :: DYNAMO | eff) (QueryResult Photo)
listFirst (Client cli) q =
  handleQuery =<< Dynamo.query do
    DQ.tableName cli.photos.tableName
    DQ.indexName cli.photos.indexNamePartCreatedAt
    DQ.descending
    DQ.keyCondition $ DQ.eq_ "#part" 0
    q

listFirst' :: forall eff. Client -> Aff (dynamo :: DYNAMO | eff) (QueryResult Photo)
listFirst' cli = listFirst cli $ pure unit

listNext :: forall eff. Client -> TableKey -> DQ.Builder Unit -> Aff (dynamo :: DYNAMO | eff) (QueryResult Photo)
listNext (Client cli) key q =
  handleQuery =<< Dynamo.query do
    DQ.tableName cli.photos.tableName
    DQ.indexName cli.photos.indexNamePartCreatedAt
    DQ.descending
    DQ.keyCondition
      $ DQ.and_
      [ DQ.eq_ "#part" key.part
      , DQ.lt_ "#created_at" key.created_at
      ]
    q

listNext' :: forall eff. Client -> TableKey -> Aff (dynamo :: DYNAMO | eff) (QueryResult Photo)
listNext' cli key = listNext cli key $ pure unit

handleQuery :: forall eff a. Decode a => Foreign -> Aff eff (QueryResult a)
handleQuery res =
  either (throwError <<< error <<< show) pure $ runExcept $ readResult res

  where
    readResult v = do
      items <- traverse decode =<< readArray =<< v ! "Items"
      count <- decode =<< v ! "Count"
      lastKey <- traverse readTableKey =<< readNullOrUndefined =<< v ! "LastEvaluatedKey"
      pure { items, count, lastKey }

    readTableKey v = do
      id <- decode =<< v ! "id"
      part <- decode =<< v ! "part"
      created_at <- decode =<< v ! "created_at"
      pure { id, part, created_at }


find :: forall eff. Client -> String -> Aff (dynamo :: DYNAMO | eff) Photo
find (Client cli) id =
  handle =<< Dynamo.get
    { "TableName": cli.photos.tableName
    , "Key": encode $ StrMap.singleton "id" id
    }
  where
    handle res =
      either (throwError <<< error <<< show) pure $ runExcept $ readResult res
    readResult res = decode $ res."Item"

create :: forall eff. Client -> Photo -> Aff (dynamo :: DYNAMO | eff) Unit
create = update

update :: forall eff. Client -> Photo -> Aff (dynamo :: DYNAMO | eff) Unit
update (Client cli) photo =
  Dynamo.put
    { "TableName": cli.photos.tableName
    , "Item": encode photo
    }

destroy :: forall eff. Client -> Photo -> Aff (dynamo :: DYNAMO | eff) Unit
destroy (Client cli) (Photo { id }) =
  Dynamo.delete
    { "TableName": cli.photos.tableName
    , "Key": encode $ StrMap.singleton "id" id
    }

count :: forall eff. Client -> Aff (dynamo :: DYNAMO | eff) Int
count (Client cli) = Dynamo.count cli.photos.tableName
