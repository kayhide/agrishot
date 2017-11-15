module Dev
       ( module Prelude
       , display
       , run
       , onLocal
       , onRemote
       , photos
       ) where

import Prelude

import Aws.Config as AwsConfig
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (either)
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (decode)
import Data.Traversable (traverse, traverse_)
import Model.Photo (Photo)


display :: forall eff a. Show a => a -> Aff (console :: CONSOLE | eff) Unit
display = logShow

run :: forall eff a. Aff eff a -> Eff eff Unit
run aff = launchAff_ aff


type AppEffs = (dynamo :: DYNAMO, console :: CONSOLE, exception :: EXCEPTION)

onLocal :: Eff AppEffs Unit
onLocal =
  Dynamo.setup =<< AwsConfig.build conf
  where
    conf = { region: "ap-northeast-1", endpoint: "http://localhost:4569", accessKeyId: "", secretAccessKey: "" }

onRemote :: Eff AppEffs Unit
onRemote =
  Dynamo.setup =<< AwsConfig.build <<< conf =<< AwsConfig.readCredentials "agrishot-dev-deploy"
  where
    conf = { region: "ap-northeast-1", credentials: _ }

photos :: Aff AppEffs (Array Photo)
photos = do
  res <- Dynamo.scan { "TableName": "agrishot-dev-photos" }
  either throw pure $ runExcept $ traverse decode res."Items"
  where
    throw errs = do
      traverse_ (throwError <<< error <<< renderForeignError) errs
      pure []

