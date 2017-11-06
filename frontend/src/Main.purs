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

type AppConfig =
  { facebookAppId :: String
  , awsIdentityPoolId :: String
  }

main :: Eff (HA.HalogenEffects (dynamo :: DYNAMO, meta :: META, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  config <- liftEff readConfig
  initFacebook $ FB.defaultConfig config.facebookAppId
  liftEff $ Dynamo.setup dynamoAwsConfig
  io <- runUI ui unit body
  io.query $ action DynamoUI.Scan
  where
    ui = DynamoUI.ui dynamoAwsConfig "agrishot-test-photos"

readConfig :: forall eff. Eff (meta :: META, exception :: EXCEPTION | eff) AppConfig
readConfig = do
  facebookAppId <- Meta.get "FACEBOOK_APP_ID"
  awsIdentityPoolId <- Meta.get "AWS_IDENTITY_POOL_ID"
  pure $ { facebookAppId, awsIdentityPoolId }

initFacebook :: forall eff. FB.Config -> Aff (console :: CONSOLE | eff) Unit
initFacebook config = do
  sdk <- FB.init config
  info <- FB.loginStatus sdk
  logShow info
