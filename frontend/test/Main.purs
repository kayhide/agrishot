module Test.Main where

import Prelude

import Aws.Config as AwsConfig
import Aws.Dynamo (DYNAMO, ScanResult)
import Aws.Dynamo as Dynamo
import Aws.Dynamo.Query as Query
import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, errorShow)
import Control.Monad.Eff.Exception (EXCEPTION, error, try)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (decode)
import Data.Foreign.Generic (encodeJSON)
import Data.List.NonEmpty as NEL
import Data.Traversable (traverse, traverse_)
import Dom.Meta (META)
import Dom.Meta as Meta
import Model.Photo (Photo)


type AppEffs = (meta :: META, dynamo :: DYNAMO, console :: CONSOLE, exception :: EXCEPTION)

main :: Eff AppEffs Unit
main = runAff_ errorShow do
  testDynamo
  testDynamoQuery
  testMeta

testDynamo :: Aff AppEffs Unit
testDynamo = do
  liftEff $ Dynamo.setup =<< AwsConfig.build conf
  res <- Dynamo.scan opts
  photos <- getItems (res :: ScanResult Foreign)."Items"
  traverse_ logShow photos

  where
    conf = { region: "local", endpoint: "http://localhost:4569", accessKeyId: "", secretAccessKey: "" }
    opts = { "TableName": "agrishot-test-photos" }

    getItems :: forall eff_. Array Foreign -> Aff eff_ (Array Photo)
    getItems objs =
      case runExcept (traverse decode objs) of
        Right xs -> pure xs
        Left err -> throwError $ error $ show $ NEL.head err

testDynamoQuery :: Aff AppEffs Unit
testDynamoQuery = do
  log $ encodeJSON q
  res <- Dynamo.query q
  pure unit
  where
    q = Query.build do
      Query.tableName "agrishot-test-photos"
      Query.indexName "agrishot-test-photos-part-created_at"
      Query.descending
      Query.limit 1

testMeta :: Aff AppEffs Unit
testMeta = do
  x <- liftEff $ try $ Meta.get "xxx"
  case x of
    Right x_ -> logShow x_
    Left err -> do
      log "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
      logShow err
