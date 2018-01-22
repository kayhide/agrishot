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
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Model.Photo (Photo)


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

listFirst :: forall eff. Client -> Aff (dynamo :: DYNAMO | eff) (QueryResult Photo)
listFirst (Client cli) =
  handleQuery =<< Dynamo.query do
    DQ.tableName cli.photos.tableName
    DQ.indexName cli.photos.indexNamePartCreatedAt
    DQ.descending
    DQ.keyCondition $ DQ.eq_ "#part" 0

listNext :: forall eff. Client -> TableKey -> Aff (dynamo :: DYNAMO | eff) (QueryResult Photo)
listNext (Client cli) key =
  handleQuery =<< Dynamo.query do
    DQ.tableName cli.photos.tableName
    DQ.indexName cli.photos.indexNamePartCreatedAt
    DQ.descending
    DQ.keyCondition
      $ DQ.and_
      [ DQ.eq_ "#part" key.part
      , DQ.lt_ "#created_at" key.created_at
      ]

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
