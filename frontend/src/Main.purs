module Main where

import Prelude

import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Component.DynamoUI as DynamoUI
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Dom.Meta (META)
import Dom.Meta as Meta
import Facebook.Sdk as FB
import Halogen (action)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

dynamoAwsConfig :: Dynamo.AwsConfig
dynamoAwsConfig =
  { region: "local"
  , endpoint: "http://localhost:4569"
  , accessKeyId: "xxxx"
  , secretAccessKey: "xxxx"
  }

main :: Eff (HA.HalogenEffects (dynamo :: DYNAMO, meta :: META, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  facebook_app_id <- liftEff $ Meta.get "FACEBOOK_APP_ID"
  logShow facebook_app_id
  initFacebook $ FB.defaultConfig facebook_app_id
  liftEff $ Dynamo.setup dynamoAwsConfig
  io <- runUI ui unit body
  io.query $ action DynamoUI.Scan
  where
    ui = DynamoUI.ui dynamoAwsConfig "agrishot-test-photos"

initFacebook :: forall e. FB.Config -> Aff (console :: CONSOLE | e) Unit
initFacebook config = do
  sdk <- FB.init config
  info <- FB.loginStatus sdk
  logShow info
