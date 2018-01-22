module Test.Main where

import Prelude

import Aws.Config as AwsConfig
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, errorShow)
import Control.Monad.Eff.Exception (EXCEPTION, try)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (Either(..))
import Dom.Meta (META)
import Dom.Meta as Meta
import Test.Aws.Dynamo as AwsDynamo
import Test.Aws.Dynamo.Query as AwsDynamoQuery


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
  AwsDynamoQuery.test
  testMeta


testMeta :: Aff AppEffs Unit
testMeta = do
  x <- liftEff $ try $ Meta.get "xxx"
  case x of
    Right x_ -> logShow x_
    Left err -> do
      log "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
      logShow err
