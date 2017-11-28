module Component.MainUI where

import Prelude

import Aws.Cognito (COGNITO)
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Component.LoginUI as LoginUI
import Component.NoticeUI as NoticeUI
import Component.PhotoListUI as PhotoListUI
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Now as Now
import Data.Array as Array
import Data.DateTime.Locale (Locale(..), LocaleName(..))
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Minutes(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


type AppConfig =
  { stage :: String
  , facebookAppId :: String
  , awsRegion :: String
  , awsIdentityPoolId :: String
  }

data Query a
  = Initialize a
  | HandleNotice NoticeUI.Message a
  | HandleLogin LoginUI.Message a
  | HandlePhotoList PhotoListUI.Message a
  | RequestScanPhotoList a
  | CheckPhotoListState a

type State =
  { appConfig :: AppConfig
  , photosCount :: Maybe Int
  , awsAuthenticated :: Boolean
  , locale :: Locale
  }

type Input = AppConfig

type Message = Void

data NoticeSlot = NoticeSlot
derive instance eqNoticeSlot :: Eq NoticeSlot
derive instance ordNoticeSlot :: Ord NoticeSlot

data LoginSlot = LoginSlot
derive instance eqLoginSlot :: Eq LoginSlot
derive instance ordLoginSlot :: Ord LoginSlot

data PhotoListSlot = PhotoListSlot
derive instance eqPhotoListSlot :: Eq PhotoListSlot
derive instance ordPhotoListSlot :: Ord PhotoListSlot

type ChildQuery = Coproduct3 NoticeUI.Query LoginUI.Query PhotoListUI.Query
type ChildSlot = Either3 NoticeSlot LoginSlot Unit

cpNotice :: CP.ChildPath NoticeUI.Query ChildQuery NoticeSlot ChildSlot
cpNotice = CP.cp1

cpLogin :: CP.ChildPath LoginUI.Query ChildQuery LoginSlot ChildSlot
cpLogin = CP.cp2

cpPhotoList :: CP.ChildPath PhotoListUI.Query ChildQuery Unit ChildSlot
cpPhotoList = CP.cp3

type Eff_ eff = Aff (cognito :: COGNITO, dynamo :: DYNAMO, now :: NOW | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleParentComponent
    { initialState: { appConfig: _
                    , photosCount: Nothing
                    , awsAuthenticated: false
                    , locale: Locale (Just (LocaleName "GMT")) (Minutes 0.0)
                    }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
render state =
  HH.div_
  [
    HH.nav
    [ HP.class_ $ H.ClassName "navbar navbar-dark bg-dark" ]
    [
      HH.span
      [ HP.class_ $ H.ClassName "navbar-brand mb-0" ]
      [ HH.text "Agrishot Admin" ]
    , HH.slot' cpLogin LoginSlot LoginUI.ui loginConfig $ HE.input HandleLogin
    ]
  , HH.slot' cpNotice NoticeSlot NoticeUI.ui unit $ HE.input HandleNotice
  , HH.main
    [ HP.class_ $ H.ClassName "container mt-2" ]
    [
      HH.h1_
      [ HH.text "Photo List" ]
    , HH.p_
      [ HH.text photosCount_ ]
    , HH.button
      [ HP.class_ $ H.ClassName updateButtonClass
      , HP.title "Update"
      , HE.onClick (HE.input_ RequestScanPhotoList)
      ]
      [ HH.text "Update" ]
    , renderPhotoList state.awsAuthenticated state.locale
    ]
  ]

  where
    photosCount_ = maybe "(unknown)" show state.photosCount
    loginConfig =
      { awsRegion: state.appConfig.awsRegion
      , awsIdentityPoolId: state.appConfig.awsIdentityPoolId
      , facebookAppId: state.appConfig.facebookAppId
      }

    updateButtonClass =
      "btn btn-outline-primary mb-2" <>
      if state.awsAuthenticated
      then ""
      else " disabled"

    renderPhotoList false _ = HH.div_ []
    renderPhotoList true locale =
      HH.slot' cpPhotoList unit PhotoListUI.ui uiInput $ HE.input HandlePhotoList
      where
        uiInput =
          { tableName: "agrishot-" <> state.appConfig.stage <> "-photos"
          , indexNamePartCreatedAt: "agrishot-" <> state.appConfig.stage <> "-photos-part-created_at"
          , locale
          }

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    locale <- H.liftEff Now.locale
    H.modify _{ locale = locale }
    pure next

  HandleNotice (NoticeUI.Closed i) next -> do
    pure next

  HandleLogin (LoginUI.Authenticated awsConfig) next -> do
    postInfo "Authenticated."
    H.liftEff $ Dynamo.setup awsConfig
    H.modify _{ awsAuthenticated = true }
    eval $ RequestScanPhotoList next

  HandleLogin (LoginUI.Failed s) next -> do
    postAlert s
    H.modify _{ awsAuthenticated = false }
    pure next

  HandlePhotoList (PhotoListUI.Scanned photos) next -> do
    H.modify _{ photosCount = Just (Array.length photos) }
    postInfo "PhotoList Updated."
    pure next

  HandlePhotoList (PhotoListUI.Failed s) next -> do
    postAlert "Failed to access database."
    postAlert "Try login again."
    pure next

  RequestScanPhotoList next -> do
    void $ H.query' cpPhotoList unit $ H.action PhotoListUI.Scan
    pure next

  CheckPhotoListState next -> do
    photos <- H.query' cpPhotoList unit $ H.request PhotoListUI.GetState
    H.modify _{ photosCount = (Array.length <<< _.items) <$> photos }
    pure next


  where
    postNotice notice =
      void $ H.query' cpNotice NoticeSlot $ H.action $ NoticeUI.Post notice
    postInfo s = postNotice $ NoticeUI.Info s
    postAlert s = postNotice $ NoticeUI.Alert s
