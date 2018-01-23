module Dev
       ( module Prelude
       , display
       , run
       , onLocal
       , onRemote
       ) where

import Prelude

import Aws.Config as AwsConfig
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)


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
