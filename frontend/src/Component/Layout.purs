module Component.Layout where

import Prelude

import Api as Api
import Aws.Cognito (COGNITO)
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Component.LoginUI as LoginUI
import Component.NoticeUI as NoticeUI
import Component.PestListPage as PestListPage
import Component.PhotoListPage as PhotoListPage
import Component.Route as R
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Now as Now
import DOM (DOM)
import Data.Const (Const)
import Data.DateTime.Locale (Locale(..), LocaleName(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String as String
import Data.Time.Duration (Minutes(..))
import Data.UUID (GENUUID)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Data.Prism (type (<\/>), type (\/))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing.Hash as Routing


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
  | HandlePhotoList PhotoListPage.Message a
  | HandlePestList PestListPage.Message a
  | Goto R.Location a

type State =
  { appConfig :: AppConfig
  , awsAuthenticated :: Boolean
  , locale :: Locale
  , location :: R.Location
  }

type Input = AppConfig

type Message = Void

type ChildQuery
  = NoticeUI.Query
    <\/> LoginUI.Query
    <\/> PhotoListPage.Query
    <\/> PestListPage.Query
    <\/> Const Void

type ChildSlot
  = NoticeUI.Slot
    \/ LoginUI.Slot
    \/ PhotoListPage.Slot
    \/ PestListPage.Slot
    \/ Void

cpNotice :: CP.ChildPath NoticeUI.Query ChildQuery NoticeUI.Slot ChildSlot
cpNotice = CP.cp1

cpLogin :: CP.ChildPath LoginUI.Query ChildQuery LoginUI.Slot ChildSlot
cpLogin = CP.cp2

cpPhotoList :: CP.ChildPath PhotoListPage.Query ChildQuery PhotoListPage.Slot ChildSlot
cpPhotoList = CP.cp3

cpPestList :: CP.ChildPath PestListPage.Query ChildQuery PestListPage.Slot ChildSlot
cpPestList = CP.cp4


type Eff_ eff = Aff (cognito :: COGNITO, dynamo :: DYNAMO, dom :: DOM, now :: NOW, uuid :: GENUUID | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

initialState :: Input -> State
initialState appConfig =
  { appConfig
  , awsAuthenticated: false
  , locale: Locale (Just (LocaleName "GMT")) (Minutes 0.0)
  , location: R.Home
  }


render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
render state =
  HH.div_
  [
    renderNavbar
  , HH.slot' cpNotice NoticeUI.Slot NoticeUI.ui unit $ const Nothing
  , HH.main
    [ HP.class_ $ H.ClassName "container mt-2 mb-5" ]
    [
      renderPage state.location
    ]
  ]

  where
    loginConfig =
      { awsRegion: state.appConfig.awsRegion
      , awsIdentityPoolId: state.appConfig.awsIdentityPoolId
      , facebookAppId: state.appConfig.facebookAppId
      }
    client = Api.makeClient state.appConfig.stage
    locale = state.locale

    renderNavbar =
      HH.nav
      [ HP.class_ $ H.ClassName "navbar navbar-expand navbar-dark bg-dark" ]
      [
        HH.a
        [ HP.class_ $ H.ClassName "navbar-brand mb-0 d-none d-sm-block"
        , HP.href "#/"
        ]
        [ HH.text "Agrishot Admin" ]
      , HH.a
        [ HP.class_ $ H.ClassName "btn btn-sm btn-outline-secondary mr-3 d-block d-sm-none"
        , HP.href "#/"
        ]
        [ HH.text "A" ]
      , HH.ul
        [ HP.class_ $ H.ClassName "navbar-nav mr-auto" ]
        [
          renderMenuItem R.PhotoListPage "Photos" "picture-o"
        , renderMenuItem R.PestListPage "Pests" "bug"
        ]
      , HH.slot' cpLogin LoginUI.Slot LoginUI.ui loginConfig $ HE.input HandleLogin
      ]

    renderMenuItem location text icon =
      HH.li
      [ HP.class_ $ H.ClassName "nav-item" ]
      [
        HH.a
        [ HP.class_ $ H.ClassName "nav-link"
        , HP.href $ R.path location
        ]
        $ renderTextOrIcon text icon
      ]

    renderTextOrIcon text icon =
      [
        HH.span
        [ HP.class_ $ H.ClassName "d-none d-sm-inline" ]
        [ HH.text text ]
      , HH.i
        [ HP.class_ $ H.ClassName $ "fa fa-fw fa-" <> icon <> " d-inline d-sm-none" ]
        []
      ]

    renderPage = case _ of
      R.Home ->
        withAuthentication
        $ HH.ul
        [ HP.class_ $ H.ClassName "nav flex-column" ]
        [
          HH.li
          [ HP.class_ $ H.ClassName "nav-item" ]
          [
            HH.a
            [ HP.class_ $ H.ClassName "nav-link"
            , HP.href $ R.path $ R.PhotoListPage
            ]
            [
              HH.i [ HP.class_ $ H.ClassName "fa fa-fw fa-picture-o mr-2" ] []
            , HH.text "Photos"
            ]
          , HH.a
            [ HP.class_ $ H.ClassName "nav-link"
            , HP.href $ R.path $ R.PestListPage
            ]
            [
              HH.i [ HP.class_ $ H.ClassName "fa fa-fw fa-bug mr-2" ] []
            , HH.text "Pests"
            ]
          ]
        ]

      R.PhotoListPage ->
        withAuthentication
        $ HH.slot' cpPhotoList PhotoListPage.Slot PhotoListPage.ui { client, locale }
        $ HE.input HandlePhotoList

      R.PestListPage ->
        withAuthentication
        $ HH.slot' cpPestList PestListPage.Slot PestListPage.ui { client, locale }
        $ HE.input HandlePestList

    withAuthentication html =
      if state.awsAuthenticated
      then html
      else HH.div_ []

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    locale <- H.liftEff Now.locale
    H.modify _{ locale = locale }

    hash <- H.liftEff $ Routing.getHash
    when (String.null hash) $
      H.liftEff $ Routing.setHash "/photos"

    pure next

  HandleNotice (NoticeUI.Closed i) next -> do
    pure next

  HandleLogin (LoginUI.Authenticated awsConfig) next -> do
    postInfo "Authenticated."
    H.liftEff $ do
      Dynamo.verbose true
      Dynamo.setup awsConfig
    H.modify _{ awsAuthenticated = true }
    pure next

  HandleLogin (LoginUI.Failed s) next -> do
    postAlert s
    H.modify _{ awsAuthenticated = false }
    pure next

  HandlePhotoList (PhotoListPage.Failed s) next -> do
    postAlert s
    pure next

  HandlePestList (PestListPage.Failed s) next -> do
    postAlert s
    pure next

  Goto loc next -> do
    H.modify _{ location = loc }
    pure next

  where
    postNotice notice =
      void $ H.query' cpNotice NoticeUI.Slot $ H.action $ NoticeUI.Post notice
    postInfo s = postNotice $ NoticeUI.Info s
    postAlert s = postNotice $ NoticeUI.Alert s

