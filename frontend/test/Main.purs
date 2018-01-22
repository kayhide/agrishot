module Test.Main where

import Prelude

import Aws.Config as AwsConfig
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, errorShow)
import Control.Monad.Eff.Exception (EXCEPTION, try)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as Array
import Data.Either (fromLeft)
import Data.Maybe (fromJust)
import Data.String (Pattern(..))
import Data.String as String
import Dom.Meta (META)
import Dom.Meta as Meta
import Partial.Unsafe (unsafePartial)
import Test.Api.Photos as ApiPhotos
import Test.Aws.Dynamo as AwsDynamo
import Test.Aws.Dynamo.Query as AwsDynamoQuery
import Test.QuickCheck (quickCheck', (===))


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
  ApiPhotos.test
  testMeta


testMeta :: Aff AppEffs Unit
testMeta = liftEff do
  x <- try $ Meta.get "xxx"
  let err = unsafePartial $ fromLeft $ x
      first = unsafePartial $ fromJust $ Array.head $ String.split (Pattern "\n") $ show err
  quickCheck' 1 (first === "Error: Meta not found: xxx")
