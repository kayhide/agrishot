module Api where

import Prelude

import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Aws.Dynamo.Query as DQ
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (Foreign, readArray, readNullOrUndefined)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Index ((!))
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)


newtype Client =
  Client
  { photos ::
    { tableName :: String
    , indexNamePartCreatedAt :: String
    }
  , pests ::
    { tableName :: String
    }
  }

makeClient :: String -> Client
makeClient stage =
  Client
  { photos:
    { tableName: "agrishot-" <> stage <> "-photos"
    , indexNamePartCreatedAt: "agrishot-" <> stage <> "-photos-part-created_at"
    }
  , pests:
    { tableName: "agrishot-" <> stage <> "-pests"
    }
  }


data Persistence = NotPersisted | Persisted

data Entity a = Entity Persistence a

_Entity :: forall a. Lens' (Entity a) a
_Entity = lens (\(Entity _ x) -> x) (\(Entity p _) x -> Entity p x)

isPersisted :: forall a. Entity a -> Boolean
isPersisted (Entity NotPersisted _) = false
isPersisted (Entity Persisted _) = true

type QueryResult a k =
  { items :: Array (Entity a)
  , count :: Int
  , lastKey :: Maybe k
  }

handleQuery ::
  forall eff a k.
  Decode a => DQ.TableKey k =>
  Foreign -> Aff (dynamo :: DYNAMO | eff) (QueryResult a k)
handleQuery res =
  either (throwError <<< error <<< show) pure $ runExcept $ readResult res

  where
    readResult v = do
      items <- traverse decode =<< readArray =<< v ! "Items"
      count_ <- decode =<< v ! "Count"
      lastKey <- traverse decode =<< readNullOrUndefined =<< v ! "LastEvaluatedKey"
      pure { items: Entity Persisted <$> items
           , count: count_
           , lastKey
           }


scanFirst ::
  forall eff k a.
  DQ.TableKey k => Decode a =>
  DQ.Builder k Unit -> Aff (dynamo :: DYNAMO | eff) (QueryResult a k)
scanFirst q =
  handleQuery =<< Dynamo.scan q

scanNext ::
  forall eff k a.
  DQ.TableKey k => Decode a =>
  k -> DQ.Builder k Unit -> Aff (dynamo :: DYNAMO | eff) (QueryResult a k)
scanNext key q =
  handleQuery =<< Dynamo.scan do
    DQ.exclusiveStartKey key
    q

queryFirst ::
  forall eff k a.
  DQ.TableKey k => Decode a =>
  DQ.Builder k Unit -> Aff (dynamo :: DYNAMO | eff) (QueryResult a k)
queryFirst q =
  handleQuery =<< Dynamo.query q

queryNext ::
  forall eff k a.
  DQ.TableKey k => Decode a =>
  k -> DQ.Builder k Unit -> Aff (dynamo :: DYNAMO | eff) (QueryResult a k)
queryNext key q =
  handleQuery =<< Dynamo.query do
    DQ.exclusiveStartKey key
    q

type TableName = String

find ::
  forall eff k a.
  Encode k => Decode a =>
  TableName -> k -> Aff (dynamo :: DYNAMO | eff) (Entity a)
find tableName key =
  handle =<< Dynamo.get
    { "TableName": tableName
    , "Key": encode key
    }
  where
    handle res =
      either (throwError <<< error <<< show) pure $ runExcept $ readResult res
    readResult res = Entity Persisted <$> decode res."Item"

create ::
  forall eff a.
  Encode a =>
  TableName -> a -> Aff (dynamo :: DYNAMO | eff) Unit
create = update

update ::
  forall eff a.
  Encode a =>
  TableName -> a -> Aff (dynamo :: DYNAMO | eff) Unit
update tableName item =
  Dynamo.put
    { "TableName": tableName
    , "Item": encode item
    }

destroy ::
  forall eff k.
  Encode k =>
  TableName -> k -> Aff (dynamo :: DYNAMO | eff) Unit
destroy tableName key =
  Dynamo.delete
    { "TableName": tableName
    , "Key": encode key
    }

count :: forall eff. String -> Aff (dynamo :: DYNAMO | eff) Int
count = Dynamo.count
