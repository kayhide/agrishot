module Component.MainUI where

import Prelude

import Aws.Dynamo (DYNAMO)
import Component.PhotoListUI as PhotoListUI
import Control.Monad.Aff (Aff)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
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
  = HandlePhotoList PhotoListUI.Message a
  | InputPhotoList PhotoListUI.Input a
  | RequestScanPhotoList a
  | CheckPhotoListState a

type State =
  { appConfig :: AppConfig
  , photosCount :: Maybe Int
  }

type Input = AppConfig

type Message = Void

data Slot = PhotoListSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type Eff_ eff = Aff (dynamo :: DYNAMO | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.parentComponent
    { initialState: { appConfig: _, photosCount: Nothing }
    , render
    , eval
    , receiver: const Nothing
    }

render :: forall eff. State -> H.ParentHTML Query PhotoListUI.Query Slot (Eff_ eff)
render state =
  HH.div_
  [
    HH.nav
    [ HP.classes [ H.ClassName "navbar", H.ClassName "navbar-dark", H.ClassName "bg-dark" ] ]
    [
      HH.span
      [ HP.classes [ H.ClassName "navbar-brand",H.ClassName "mb-0" ] ]
      [ HH.text "Agrishot Admin" ]
    ]
  , HH.main
    [ HP.class_ $ H.ClassName "container" ]
    [
      HH.h1_
      [ HH.text "Photo List" ]
    , HH.p_
      [ HH.text photosCount_ ]
    , HH.button
      [ HP.title "Update"
      , HE.onClick (HE.input_ RequestScanPhotoList)
      , HP.classes [ H.ClassName "btn", H.ClassName "btn-outline-primary", H.ClassName "mb-2" ]
      ]
      [ HH.text "Update" ]
    , HH.slot PhotoListSlot PhotoListUI.ui tableName (HE.input HandlePhotoList)
    ]
  ]

  where
    photosCount_ = maybe "(unknown)" show state.photosCount
    tableName = "agrishot-" <> state.appConfig.stage <> "-photos"

eval :: forall eff. Query ~> H.ParentDSL State Query PhotoListUI.Query Slot Message (Eff_ eff)
eval = case _ of
  HandlePhotoList (PhotoListUI.Scanned photos) next -> do
    H.modify (_ { photosCount = Just (Array.length photos) })
    pure next

  InputPhotoList s next -> do
    void $ H.query PhotoListSlot $ H.action $ PhotoListUI.HandleInput s
    pure next

  RequestScanPhotoList next -> do
    void $ H.query PhotoListSlot $ H.action PhotoListUI.Scan
    pure next

  CheckPhotoListState next -> do
    photos <- H.query PhotoListSlot $ H.request PhotoListUI.GetState
    H.modify (_ { photosCount = (Array.length <<< _.items) <$> photos })
    pure next
