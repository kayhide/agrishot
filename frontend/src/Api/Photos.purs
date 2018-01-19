module Api.Photos where

import Prelude

import Api (Client(..))
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Aws.Dynamo.Query as DQ
import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, decode)
import Data.Traversable (traverse)
import Model.Photo (Photo)


index :: forall eff. Client -> Aff (dynamo :: DYNAMO | eff) (Array Photo)
index (Client cli) =
  handleQuery =<< Dynamo.query do
    DQ.tableName cli.photos.tableName
    DQ.indexName cli.photos.indexNamePartCreatedAt
    DQ.descending
    DQ.keyCondition $ DQ.eq_ "#part" 0


handleQuery :: forall eff a. Decode a => Dynamo.QueryResult Foreign -> Aff eff (Array a)
handleQuery res = case (runExcept readItems) of
  Right x -> pure x
  Left err -> throwError <<< error $ show err

  where
    readItems = traverse decode res."Items"
