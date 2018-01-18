module Component.LoginUI where

import Prelude

import Aws.Cognito (COGNITO)
import Aws.Cognito as Cognito
import Aws.Config (AwsConfig)
import Control.Monad.Aff (Aff, Error, attempt)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Maybe.Trans (lift)
import Control.Monad.State (class MonadState)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap)
import Facebook.Sdk as FB
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


type LoginConfig =
  { awsRegion :: String
  , awsIdentityPoolId :: String
  , facebookAppId :: String
  }

type State =
  { config :: LoginConfig
  , facebookInitialized :: Boolean
  , facebookSdk :: Maybe FB.Sdk
  , facebookAccessToken :: Maybe String
  , facebookUserId :: String
  , awsConfig :: Maybe AwsConfig
  }

data Query a
  = Initialize a
  | CheckFacebook a
  | LoginFacebook a
  | Authenticate a

type Input = LoginConfig

data Message
  = Authenticated AwsConfig
  | Failed String


type Eff_ eff = Aff (cognito :: COGNITO | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleComponent
    { initialState: { config: _
                    , facebookInitialized: false
                    , facebookSdk: Nothing
                    , facebookAccessToken: Nothing
                    , facebookUserId: "..."
                    , awsConfig: Nothing
                    }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div
  [ HP.class_ $ H.ClassName "form-inline my-2 my-lg-0" ]
  [
    HH.span
    [ HP.class_ $ H.ClassName "navbar-text mr-2" ]
    [ HH.text $ state.facebookUserId ]
  , HH.button
    [ HP.class_ $ H.ClassName $ buttonClass
    , HE.onClick $ HE.input_ LoginFacebook
    ]
    [
      HH.i [ HP.class_ $ H.ClassName "fa fa-facebook fa-fw" ] []
    ]
  ]

  where
    buttonClass =
      case state.facebookAccessToken of
        Just _ -> "btn btn-sm btn-secondary"
        Nothing -> "btn btn-sm btn-outline-secondary"

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval (Initialize next) = do
  config <- FB.defaultConfig <$> H.gets _.config.facebookAppId
  sdk <- H.liftAff $ attempt $ FB.init config

  case sdk of
    Left _ -> do
      H.raise $ Failed $ "Facebook init failed with: " <> (unwrap config).appId
      pure next

    Right sdk_ -> do
      H.modify _{ facebookInitialized = true, facebookSdk = Just sdk_ }
      eval $ CheckFacebook next

eval (CheckFacebook next) = do
  res <- runExceptT do
    sdk <- verifyFacebookSdk
    FB.StatusInfo info <- lift $ H.liftAff $ FB.loginStatus sdk
    FB.AuthResponse auth <- onNothing "Facebook login not ready" info.authResponse
    let FB.AccessToken token = auth.accessToken
        FB.UserId userId = auth.userId
    lift $ H.modify _{ facebookAccessToken = Just token, facebookUserId = userId }

  case res of
    Left err -> do
      H.raise $ Failed err
      pure next
    Right _ -> do
      eval $ Authenticate next

eval (LoginFacebook next) = do
  res <- runExceptT do
    sdk <- verifyFacebookSdk
    FB.StatusInfo info <- lift $ H.liftAff $ FB.login sdk $ FB.LoginOptions{ scopes: [ FB.Scope "public_profile" ] }
    FB.AuthResponse auth <- onNothing "Facebook login failed" info.authResponse
    let FB.AccessToken token = auth.accessToken
        FB.UserId userId = auth.userId
    lift $ H.modify _{ facebookAccessToken = Just token, facebookUserId = userId }

  case res of
    Left err -> do
      H.raise $ Failed err
      pure next
    Right _ -> do
      eval $ Authenticate next

eval (Authenticate next) = do
  config <- H.gets _.config
  res <- runExceptT do
    token <- verifyFacebookAccessToken
    lift $ H.liftEff do
      Cognito.setRegion config.awsRegion
      Cognito.setIdentityPoolId config.awsIdentityPoolId
      Cognito.setFacebookToken token
    awsConfig <- verifyAuth =<< H.liftAff (attempt Cognito.authenticate)
    lift $ do
      H.modify _{ awsConfig = Just awsConfig }
      H.raise $ Authenticated awsConfig

  either (H.raise <<< Failed) pure res

  pure next


onNothing :: forall m. Monad m => String -> Maybe ~> ExceptT String m
onNothing s = maybe (throwError s) pure

verifyFacebookSdk :: forall m. MonadState State m => ExceptT String m FB.Sdk
verifyFacebookSdk =
  onNothing "Facebook not initialized" =<< (lift $ H.gets _.facebookSdk)

verifyFacebookAccessToken :: forall m. MonadState State m => ExceptT String m String
verifyFacebookAccessToken =
  onNothing "Facebook login not done" =<< (lift $ H.gets _.facebookAccessToken)

onLeft :: forall e m. Monad m => String -> Either e ~> ExceptT String m
onLeft s = either (throwError <<< const s) pure

verifyAuth :: forall m. Monad m => Either Error AwsConfig -> ExceptT String m AwsConfig
verifyAuth = onLeft "Authentication failed"
