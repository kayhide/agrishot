module Component.PestListPage where

import Prelude

import Api (Entity(..), _Entity, isPersisted)
import Api as Api
import Api.Pests (PestEntity)
import Api.Pests as Pests
import Aws.Dynamo (DYNAMO)
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.PestEditUI as PestEditUI
import Component.Util as Util
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, attempt)
import Control.MonadZero (guard)
import Data.Array ((:))
import Data.Array as Array
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right))
import Data.Function (on)
import Data.Lens (Lens', assign, modifying, use, view, (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Symbol (SProxy(..))
import Data.UUID (GENUUID)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model.Pest (Pest(..))
import Model.Pest as Pest


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | New a
  | Edit PestEntity a
  | Reload a
  | LoadNext a
  | HandleEdit PestEditUI.Message a

type State =
  { client :: Api.Client
  , locale :: Locale
  , items :: Array PestEntity
  , editing :: Maybe PestEntity
  , last :: Maybe Pests.TableKey
  , busy :: Boolean
  }

_items :: Lens' State (Array PestEntity)
_items = prop (SProxy :: SProxy "items")

_editing :: Lens' State (Maybe PestEntity)
_editing = prop (SProxy :: SProxy "editing")

_item :: forall r. Lens' { item :: Pest | r } Pest
_item = prop (SProxy :: SProxy "item")

_isNew :: forall r. Lens' { isNew :: Boolean | r } Boolean
_isNew = prop (SProxy :: SProxy "isNew")


type Input =
  { client :: Api.Client
  , locale :: Locale
  }

data Message
  = Failed String

type ChildQuery = PestEditUI.Query
type ChildSlot = PestEditUI.Slot

type Eff_ eff = Aff (dynamo :: DYNAMO, uuid :: GENUUID | eff)

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
initialState { client, locale } =
  { client
  , locale
  , items: []
  , editing: Nothing
  , last: Nothing
  , busy: false
  }

render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
render state =
  HH.div_ $
  [
    LoadingIndicator.render state.busy
  , HH.button
    [ HP.class_ $ H.ClassName "btn btn-outline-primary mb-2"
    , HP.title "Reload"
    , HE.onClick (HE.input_ Reload)
    ]
    [
      HH.i
      [ HP.class_ $ H.ClassName "fa fa-refresh"
      , HP.title "Reload"
      ] []
    ]
  , HH.div
    [ HP.class_ $ H.ClassName "row no-gutters" ] $
    renderNewItem : (renderItem <$> state.items)
  ]
  <> (Array.fromFoldable $ renderLoadNextButton <$> state.last)

  where
    client = state.client
    locale = state.locale

    renderLoadNextButton _ =
      HH.button
      [ HP.class_ $ H.ClassName "btn btn-sm btn-secondary mb-2"
      , HP.title "LoadNext"
      , HE.onClick $ HE.input_ LoadNext
      ]
      [
        HH.i
        [ HP.class_ $ H.ClassName "fa fa-chevron-down"
        , HP.title "LoadNext"
        ] []
      ]

    renderPestEditUI item =
      col $
      HH.slot slot PestEditUI.ui { item, client, locale } $ HE.input HandleEdit
      where
        slot = PestEditUI.Slot $ item ^. (_Entity <<< Pest._id)

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

    renderNewItem =
      maybe renderNewButton renderPestEditUI $ do
        guard =<< (not <<< isPersisted) <$> state.editing
        state.editing

    renderNewButton =
      col $
      HH.div
      [ HP.class_ $ H.ClassName "card" ]
      [
        renderTopRightButton "success" "plus" New
      , HH.div
        [ HP.class_ $ H.ClassName "card-header" ]
        [
          HH.i
          [ HP.class_ $ H.ClassName "fa fa-bug mr-2"
          ] []
        ]
      ]

    renderItem pest@(Entity _ (Pest { id })) =
      maybe (renderItemDetail pest) renderPestEditUI $ do
        guard =<< isPersisted <$> state.editing
        guard =<< eq id <<< (view $ _Entity <<< Pest._id) <$> state.editing
        pure pest

    renderItemDetail pest@(Entity _ (Pest { id, label, description, url })) =
      col $
      HH.div
      [ HP.class_ $ H.ClassName "card" ]
      [
        renderTopRightButton "secondary" "pencil" $ Edit pest
      , HH.div
        [ HP.class_ $ H.ClassName "card-header" ]
        [
          HH.i
          [ HP.class_ $ H.ClassName "fa fa-bug mr-2"
          ] []
        , HH.text label
        ]
      , HH.div
        [ HP.class_ $ H.ClassName "card-body" ]
        [
          HH.p
          [ HP.class_ $ H.ClassName "card-text small" ]
          [ HH.text description ]
        , HH.p
          [ HP.class_ $ H.ClassName "card-text text-muted small" ]
          [ HH.text id ]
        , HH.a
          [ HP.href $ url
          ]
          [
            HH.i
            [ HP.class_ $ H.ClassName "fa fa-external-link"
            ] []
          ]
        ]
      ]

    col html =
      HH.div
      [ HP.class_ $ H.ClassName "col-md-6 col-12 pb-2" ]
      [ html ]

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    eval $ Reload next

  New next -> do
    item <- H.liftEff Pests.build
    assign _editing $ Just item
    pure next

  Edit item next -> do
    assign _editing $ Just item
    pure next

  Reload next -> do
    Util.whenNotBusy_ do
      { client } <- H.get
      res <- H.liftAff $ attempt $ Pests.listFirst' client
      case res of
        Left err -> do
          H.raise $ Failed "DynamoDB scan failed"
        Right { items, lastKey } -> do
          H.modify _{ items = items, last = lastKey }

    pure next

  LoadNext next -> do
    Util.whenNotBusy_ do
      { client, last } <- H.get
      case last of
        Nothing -> pure unit
        Just last_ -> do
          res <- H.liftAff $ attempt $ Pests.listNext' client last_
          case res of
            Left err -> do
              H.raise $ Failed "DynamoDB scan failed"
            Right { items, lastKey } -> do
              items_ <- H.gets _.items
              H.modify _{ items = items_ <> items, last = lastKey }

    pure next

  HandleEdit (PestEditUI.Failed s) next -> do
    H.raise $ Failed s
    pure next

  HandleEdit (PestEditUI.Submitted item) next -> do
    items <- use _items
    let id = item ^. _Entity <<< Pest._id
        items_ = do
          i <- Array.findIndex ((_ == id) <<< view (_Entity <<< Pest._id)) items
          Array.updateAt i item items
    maybe (pure unit) (assign _items) $ items_ <|> Just (item : items)
    assign _editing Nothing
    pure next

  HandleEdit (PestEditUI.Deleted item) next -> do
    modifying _items $ Array.deleteBy ((==) `on` view (_Entity <<< Pest._id)) item
    pure next

  HandleEdit PestEditUI.Canceled next -> do
    assign _editing Nothing
    pure next
