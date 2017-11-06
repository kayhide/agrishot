module Test.Main where

import Prelude

import Aws.Dynamo (DYNAMO, ScanResult)
import Aws.Dynamo as Dynamo
import Control.Monad.Aff (Aff, Canceler, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, errorShow)
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

main :: Eff (dynamo :: DYNAMO, meta :: META, console :: CONSOLE, exception :: EXCEPTION)
        (Canceler (dynamo :: DYNAMO, meta :: META, console :: CONSOLE, exception :: EXCEPTION))
main = runAff errorShow pure do
  testDynamo
  testMeta

testDynamo :: forall eff. Aff (dynamo :: DYNAMO, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
testDynamo = do
  liftEff $ Dynamo.setup conf
  res <- Dynamo.scan conf opts
  photos <- getItems (res :: ScanResult Foreign)."Items"
  liftEff $ traverse_ (log <<< show) photos

  where
    conf = { region: "local", endpoint: "http://localhost:4569", accessKeyId: "", secretAccessKey: "" }
    opts = { "TableName": "agrishot-test-photos" }

    getItems :: forall eff_. Array Foreign -> Aff eff_ (Array Photo)
    getItems objs =
      case runExcept (traverse decode objs) of
        Right xs -> pure xs
        Left err -> throwError $ error $ show $ NEL.head err

testMeta :: forall eff. Aff (meta :: META, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
testMeta = liftEff do
  x <- try $ Meta.get "xxx"
  case x of
    Right x_ -> log x_
    Left err -> do
      log "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
      log $ show err
