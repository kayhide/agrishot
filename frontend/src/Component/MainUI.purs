module Component.MainUI where

import Prelude

import Aws.Dynamo (DYNAMO)
import Component.NoticeUI as NoticeUI
import Component.PhotoListUI as PhotoListUI
import Control.Monad.Aff (Aff)
import Data.Array as Array
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), maybe)
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
  = HandlePhotoList PhotoListUI.Message a
  | InputPhotoList PhotoListUI.Input a
  | RequestScanPhotoList a
  | CheckPhotoListState a

type State =
  { appConfig :: AppConfig
  , photosCount :: Maybe Int
  , notices :: Array { id :: Int, notice :: NoticeUI.Notice }
  , noticeLastId :: Int
  }

type Input = AppConfig

type Message = Void

data NoticeSlot = NoticeSlot Int
derive instance eqNoticeSlot :: Eq NoticeSlot
derive instance ordNoticeSlot :: Ord NoticeSlot

data PhotoListSlot = PhotoListSlot
derive instance eqPhotoListSlot :: Eq PhotoListSlot
derive instance ordPhotoListSlot :: Ord PhotoListSlot

type ChildQuery = Coproduct2 NoticeUI.Query PhotoListUI.Query
type ChildSlot = Either2 NoticeSlot Unit

cpNotice :: CP.ChildPath NoticeUI.Query ChildQuery NoticeSlot ChildSlot
cpNotice = CP.cp1

cpPhotoList :: CP.ChildPath PhotoListUI.Query ChildQuery Unit ChildSlot
cpPhotoList = CP.cp2

type Eff_ eff = Aff (dynamo :: DYNAMO | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.parentComponent
    { initialState: { appConfig: _
                    , photosCount: Nothing
                    , notices: [ { id: 0, notice: NoticeUI.Info "Hi!" }
                               , { id: 1, notice: NoticeUI.Alert "Watch!" }
                               ]
                    , noticeLastId: 1
                    }
    , render
    , eval
    , receiver: const Nothing
    }

render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
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
    [ HP.classes [ H.ClassName "container",H.ClassName "mt-2" ] ]
    [
      HH.div_ renderNotices
    , HH.h1_
      [ HH.text "Photo List" ]
    , HH.p_
      [ HH.text photosCount_ ]
    , HH.button
      [ HP.title "Update"
      , HE.onClick (HE.input_ RequestScanPhotoList)
      , HP.classes [ H.ClassName "btn", H.ClassName "btn-outline-primary", H.ClassName "mb-2" ]
      ]
      [ HH.text "Update" ]
    , HH.slot' cpPhotoList unit PhotoListUI.ui tableName $ HE.input HandlePhotoList
    ]
  ]

  where
    photosCount_ = maybe "(unknown)" show state.photosCount
    tableName = "agrishot-" <> state.appConfig.stage <> "-photos"

    renderNotices = do
      x@{ id, notice } <- state.notices
      pure $ HH.slot' cpNotice (NoticeSlot id) NoticeUI.ui x absurd

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  HandlePhotoList (PhotoListUI.Scanned photos) next -> do
    H.modify (_ { photosCount = Just (Array.length photos) })
    pure next

  InputPhotoList s next -> do
    void $ H.query' cpPhotoList unit $ H.action $ PhotoListUI.HandleInput s
    pure next

  RequestScanPhotoList next -> do
    void $ H.query' cpPhotoList unit $ H.action PhotoListUI.Scan
    pure next

  CheckPhotoListState next -> do
    photos <- H.query' cpPhotoList unit $ H.request PhotoListUI.GetState
    H.modify (_ { photosCount = (Array.length <<< _.items) <$> photos })
    pure next
