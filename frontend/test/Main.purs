module Test.Main where

import Aws.Dynamo

import Control.Monad.Aff (Aff, Canceler, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, errorShow)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (decode)
import Data.List.NonEmpty as NEL
import Data.Traversable (traverse, traverse_)
import Model.Photo (Photo)
import Prelude (bind, pure, show, ($), (<<<))

main :: Eff (console :: CONSOLE, dynamo :: DYNAMO) (Canceler (console :: CONSOLE, dynamo :: DYNAMO))
main =
  runAff errorShow pure do
    res <- scan conf opts
    photos <- getItems (res :: ScanResult Foreign)."Items"
    liftEff $ traverse_ (log <<< show) photos

  where
    conf = { region: "local", endpoint: "http://localhost:4569", accessKeyId: "", secretAccessKey: "" }
    opts = { "TableName": "agrishot-test-photos" }

    getItems :: forall eff. Array Foreign -> Aff eff (Array Photo)
    getItems objs =
      case runExcept (traverse decode objs) of
        Right xs -> pure xs
        Left err -> throwError $ error $ show $ NEL.head err
