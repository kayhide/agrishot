module Main where

import Prelude

import Aws.Cognito (COGNITO)
import Aws.Cognito as Cognito
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Component.DynamoUI as DynamoUI
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Dom.Meta (META)
import Dom.Meta as Meta
import Facebook.Sdk as FB
import Halogen (action)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

type AppConfig =
  { facebookAppId :: String
  , awsRegion :: String
  , awsIdentityPoolId :: String
  }

main :: Eff (HA.HalogenEffects (meta :: META, cognito :: COGNITO, dynamo :: DYNAMO, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  config <- liftEff readConfig
  liftEff $ Cognito.setRegion config.awsRegion
  liftEff $ Cognito.setIdentityPoolId config.awsIdentityPoolId
  FB.AccessToken token <- loginFacebook $ FB.defaultConfig config.facebookAppId
  liftEff $ Cognito.setFacebookToken token
  conf <- Cognito.authenticate
  liftEff $ Dynamo.setup conf
  let ui = DynamoUI.ui conf "agrishot-dev-photos"
  io <- runUI ui unit body
  io.query $ action DynamoUI.Scan

readConfig :: forall eff. Eff (meta :: META, exception :: EXCEPTION | eff) AppConfig
readConfig = do
  facebookAppId <- Meta.get "FACEBOOK_APP_ID"
  awsIdentityPoolId <- Meta.get "AWS_IDENTITY_POOL_ID"
  let region = Array.head $ split (Pattern ":") awsIdentityPoolId
  case region of
    Just awsRegion ->
      pure $ { facebookAppId, awsRegion, awsIdentityPoolId }
    Nothing ->
      liftEff $ throwException $ error $ "Bad aws identity pool id"

loginFacebook :: forall eff. FB.Config -> Aff (exception :: EXCEPTION | eff) FB.AccessToken
loginFacebook config = do
  sdk <- FB.init config
  FB.StatusInfo info <- FB.loginStatus sdk
  case (info.authResponse) of
    Just (FB.AuthResponse auth) -> pure auth.accessToken
    Nothing -> liftEff $ throwException $ error $ "Facebook login failed"
