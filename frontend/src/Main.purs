module Main where

import Prelude

import Aws.Cognito (COGNITO)
import Aws.Cognito as Cognito
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Component.MainUI as MainUI
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
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

type AppConfig =
  { stage :: String
  , facebookAppId :: String
  , awsRegion :: String
  , awsIdentityPoolId :: String
  }

type AppEffs = HA.HalogenEffects (meta :: META, cognito :: COGNITO, dynamo :: DYNAMO, console :: CONSOLE)

main :: Eff AppEffs Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  initApp =<< liftEff readConfig
  io <- runUI MainUI.ui unit body
  io.query $ H.action MainUI.RequestScanPhotoList

readConfig :: forall eff. Eff (meta :: META, exception :: EXCEPTION | eff) AppConfig
readConfig = do
  stage <- Meta.get "STAGE"
  facebookAppId <- Meta.get "FACEBOOK_APP_ID"
  awsIdentityPoolId <- Meta.get "AWS_IDENTITY_POOL_ID"
  let region = Array.head $ split (Pattern ":") awsIdentityPoolId
  case region of
    Just awsRegion ->
      pure $ { stage, facebookAppId, awsRegion, awsIdentityPoolId }
    Nothing ->
      liftEff $ throwException $ error $ "Bad aws identity pool id"

initApp :: forall eff. AppConfig -> Aff (cognito :: COGNITO, dynamo :: DYNAMO, exception :: EXCEPTION | eff) Unit
initApp config = do
  FB.AccessToken token <- loginFacebook $ FB.defaultConfig config.facebookAppId
  liftEff do
    Cognito.setRegion config.awsRegion
    Cognito.setIdentityPoolId config.awsIdentityPoolId
    Cognito.setFacebookToken token
  liftEff <<< Dynamo.setup =<< Cognito.authenticate

loginFacebook :: forall eff. FB.Config -> Aff (exception :: EXCEPTION | eff) FB.AccessToken
loginFacebook config = do
  sdk <- FB.init config
  FB.StatusInfo info <- FB.loginStatus sdk
  case (info.authResponse) of
    Just (FB.AuthResponse auth) -> pure auth.accessToken
    Nothing -> liftEff $ throwException $ error $ "Facebook login failed"
