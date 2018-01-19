module Test.Main where

import Prelude

import Aws.Config as AwsConfig
import Aws.Dynamo (DYNAMO, ScanResult)
import Aws.Dynamo as Dynamo
import Aws.Dynamo.Query as DQ
import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, errorShow)
import Control.Monad.Eff.Exception (EXCEPTION, error, try)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (decode)
import Data.List.NonEmpty as NEL
import Data.Traversable (traverse, traverse_)
import Dom.Meta (META)
import Dom.Meta as Meta
import Model.Photo (Photo)
import Test.Aws.Dynamo as AwsDynamo


type AppEffs = ( console :: CONSOLE
               , random :: RANDOM
               , exception :: EXCEPTION
               , dynamo :: DYNAMO
               , meta :: META
               )

setup :: Eff AppEffs Unit
setup =
  Dynamo.setup =<< AwsConfig.build conf
  where
    conf =
      { region: "local"
      , endpoint: "http://localhost:4569"
      , accessKeyId: ""
      , secretAccessKey: ""
      }

main :: Eff AppEffs Unit
main = runAff_ errorShow do
  liftEff setup
  AwsDynamo.test
  testDynamo
  testDynamoQuery
  testMeta

testDynamo :: Aff AppEffs Unit
testDynamo = do
  res <- Dynamo.scan opts
  logShow res."Count"
  photos <- getItems (res :: ScanResult Foreign)."Items"
  traverse_ logShow photos

  where
    opts = { "TableName": "agrishot-test-photos" }

getItems :: forall eff_. Array Foreign -> Aff eff_ (Array Photo)
getItems objs =
  case runExcept (traverse decode objs) of
    Right xs -> pure xs
    Left err -> throwError $ error $ show $ NEL.head err

testDynamoQuery :: Aff AppEffs Unit
testDynamoQuery = do
  res <- Dynamo.query q
  photos <- getItems res."Items"
  traverse_ logShow photos
  pure unit
  where
    q = do
      DQ.tableName "agrishot-test-photos"
      DQ.indexName "agrishot-test-photos-part-created_at"
      DQ.ascending
      DQ.limit 5
      DQ.keyCondition $
        DQ.and_
        [ DQ.eq_ "#part" 0
        , DQ.between_ "#created_at" 1511354013941.0 1511358491331.0
        ]
      DQ.filter $
        DQ.and_
        [ DQ.ne_ "#sender_id" "#image_url"
        , DQ.in_ "#sender_id" ["line:U1278409", "hoge"]
        , DQ.in_ "#sender.#provider" ["line", "hoge"]
        ]

testMeta :: Aff AppEffs Unit
testMeta = do
  x <- liftEff $ try $ Meta.get "xxx"
  case x of
    Right x_ -> logShow x_
    Left err -> do
      log "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
      logShow err
