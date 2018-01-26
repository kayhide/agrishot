module Component.PestEditUI where

import Prelude

import Api as Api
import Api.Pests as Pests
import Aws.Dynamo (DYNAMO)
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.HTML.TextArea as TextArea
import Component.HTML.TextField as TextField
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExceptT)
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right))
import Data.Lens (Lens', assign, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import I18n.Ja as Ja
import Model.Pest (Pest, PestId)
import Model.Pest as Pest


data Slot = Slot PestId
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | SetString (Lens' Pest String) String a
  | SetBoolean (Lens' Pest Boolean) Boolean a
  | Submit a
  | Delete a
  | Cancel a

type State =
  { item :: Pest
  , editing :: Pest
  , client :: Api.Client
  , locale :: Locale
  , busy :: Boolean
  }

_editing :: Lens' State Pest
_editing = prop (SProxy :: SProxy "editing")


type Input =
  { item :: Pest
  , client :: Api.Client
  , locale :: Locale
  }

data Message
  = Failed String
  | Submitted Pest
  | Deleted Pest
  | Canceled


type Eff_ eff = Aff (dynamo :: DYNAMO | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleComponent
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

initialState :: Input -> State
initialState { item, client, locale } =
    { item
    , editing: item
    , client
    , locale
    , busy: false
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div
  [ HP.class_ $ H.ClassName "card" ]
  [
    LoadingIndicator.render state.busy
  , renderTopRightButton "secondary" "times" Cancel
  , HH.div
    [ HP.class_ $ H.ClassName "card-body" ]
    [
      renderTextField "pest-label" Ja.pest_label Pest._label
    , renderTextArea "pest-description" Ja.pest_description Pest._description
    , renderTextField "pest-url" Ja.pest_url Pest._url
    ]
  , HH.div
    [ HP.class_ $ H.ClassName "card-footer" ]
    [
      HH.div
      [ HP.class_ $ H.ClassName "d-flex" ]
      [
        renderSubmitButton
      , renderDeleteButton
      ]
    ]
  ]

  where
    modified = state.editing /= state.item

    renderTextField :: String -> String -> Lens' Pest String -> H.ComponentHTML Query
    renderTextField key label attr =
      TextField.render key label (view attr state.editing) $ SetString attr

    renderTextArea :: String -> String -> Lens' Pest String -> H.ComponentHTML Query
    renderTextArea key label attr =
      TextArea.render key label (view attr state.editing) $ SetString attr

    renderTopRightButton style icon q =
        HH.button
        [ HP.class_ $ H.ClassName $ "position-absolute-right mt-2 mr-2 btn btn-sm btn-" <> style
        , HE.onClick $ HE.input_ q
        ]
        [
          HH.i
          [ HP.class_ $ H.ClassName $ "fa fa-" <> icon
          ] []
        ]

    renderSubmitButton =
      HH.button
      [ HP.class_ $ H.ClassName $
        "btn btn-primary" <> if modified then "" else " disabled"
      , HE.onClick $ HE.input_ Submit
      ]
      [ HH.text Ja.submit ]

    renderDeleteButton =
      HH.button
      [ HP.class_ $ H.ClassName $
        "ml-auto btn btn-danger" <> if modified then "" else " disabled"
      , HE.onClick $ HE.input_ Delete
      ]
      [ HH.text Ja.delete ]

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    pure next

  SetString attr v next -> do
    assign (_editing <<< attr) v
    pure next

  SetBoolean attr v next -> do
    assign (_editing <<< attr) v
    pure next

  Submit next -> do
    Util.whenNotBusy_ do
      { editing, client } <- H.get
      res <- runExceptT do
        Util.onLeft "Failed to update editing."
          =<< (H.liftAff $ attempt $ Pests.update client editing)

      case res of
        Right _ ->
          H.raise $ Submitted editing
        Left msg ->
          H.raise $ Failed msg

    pure next

  Delete next -> do
    Util.whenNotBusy_ do
      { editing, client } <- H.get
      res <- runExceptT do
        Util.onLeft "Failed to delete editing."
          =<< (H.liftAff $ attempt $ Pests.destroy client editing)

      case res of
        Right _ ->
          H.raise $ Deleted editing
        Left msg ->
          H.raise $ Failed msg

    pure next

  Cancel next -> do
    H.raise $ Canceled
    pure next
