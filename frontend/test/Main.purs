module Test.Main where

import Prelude

import Aws.Config as AwsConfig
import Aws.Dynamo (DYNAMO, ScanResult)
import Aws.Dynamo as Dynamo
import Aws.Dynamo.Expression as DE
import Aws.Dynamo.Query as DQ
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
  logShow res."Count"
  photos <- getItems (res :: ScanResult Foreign)."Items"
  traverse_ logShow photos

  where
    conf = { region: "local", endpoint: "http://localhost:4569", accessKeyId: "", secretAccessKey: "" }
    opts = { "TableName": "agrishot-dev-photos" }

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
    q = DQ.build do
      DQ.tableName "agrishot-dev-photos"
      DQ.indexName "agrishot-dev-photos-part-created_at"
      DQ.ascending
      DQ.limit 5
      DQ.keyCondition $
        DE.AND_
        [ (DE.EQ_ (DE.Path ["part"]) (DE.I 0))
        , (DE.BETWEEN_ (DE.Path ["created_at"]) (DE.N 1511354013941.0) (DE.N 1511358491331.0))
        ]
      DQ.filter $
        -- (DE.NE_ (DE.Path ["sender_id"]) (DE.Path ["image_url"]))
        -- DE.IN_ (DE.Path ["sender_id"]) [(DE.S "line:U1278409"), (DE.S "hoge")]
        DE.IN_ (DE.Path ["sender", "provider"]) [(DE.S "line"), (DE.S "hoge")]

testMeta :: Aff AppEffs Unit
testMeta = do
  x <- liftEff $ try $ Meta.get "xxx"
  case x of
    Right x_ -> logShow x_
    Left err -> do
      log "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
      logShow err
