module Main where

import Prelude

import Aws.Cognito (COGNITO)
import Aws.Dynamo (DYNAMO)
import Component.MainUI (AppConfig)
import Component.MainUI as MainUI
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Eff.Now (NOW)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Dom.Meta (META)
import Dom.Meta as Meta
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


type AppEffs = HA.HalogenEffects (meta :: META, cognito :: COGNITO, dynamo :: DYNAMO, now :: NOW, console :: CONSOLE)

main :: Eff AppEffs Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  appConfig <- liftEff readConfig
  io <- runUI MainUI.ui appConfig body
  io.query $ H.action MainUI.RequestScanPhotoList

readConfig :: Eff AppEffs AppConfig
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
