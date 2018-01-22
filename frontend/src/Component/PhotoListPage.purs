module Component.PhotoListPage where

import Prelude

import Api as Api
import Api.Photos as Photos
import Aws.Dynamo (DYNAMO)
import Component.HTML.DateTime as DateTime
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Data.Array as Array
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model.Photo (Photo(..), Sender(..))


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


data Query a
  = Initialize a
  | Reload a
  | LoadNext a

type State =
  { client :: Api.Client
  , locale :: Locale
  , items :: Array Photo
  , last :: Maybe Photos.TableKey
  , busy :: Boolean
  }

type Input =
  { client :: Api.Client
  , locale :: Locale
  }

data Message
  = Failed String


type Eff_ eff = Aff (dynamo :: DYNAMO | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleComponent
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
  , last: Nothing
  , busy: false
  }

render :: State -> H.ComponentHTML Query
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
    [ HP.class_ $ H.ClassName "row no-gutters" ]
    (renderItem <$> state.items)
  ]
  <> (Array.fromFoldable $ renderLoadNextButton <$> state.last)

  where
    renderLoadNextButton _ =
      HH.button
      [ HP.class_ $ H.ClassName "btn btn-sm btn-secondary mb-2"
      , HP.title "LoadNext"
      , HE.onClick (HE.input_ LoadNext)
      ]
      [
        HH.i
        [ HP.class_ $ H.ClassName "fa fa-chevron-down"
        , HP.title "LoadNext"
        ] []
      ]

    renderItem (Photo { id, sender_id, image_url, created_at, sender }) =
      HH.div
      [ HP.class_ $ H.ClassName "col-md-2 col-sm-4 col-6 pb-2" ]
      [ HH.div
        [ HP.class_ $ H.ClassName "card" ]
        [
          HH.a
          [ HP.href image_url, HP.target "_blank" ]
          [
            renderThumbnail image_url
          ]
        , HH.div
          [ HP.class_ $ H.ClassName "card-body" ]
          [ HH.p
            [ HP.class_ $ H.ClassName "card-text small" ] $
            [
              HH.i
              [ HP.class_ $ H.ClassName "fa fa-user"
              , HP.title sender_id
              ] []
            ] <> Array.fromFoldable (renderSender <$> sender)
          , HH.p
            [ HP.class_ $ H.ClassName "card-text text-muted small" ]
            [ DateTime.render created_at state.locale ]
          ]
        ]
      ]

    renderThumbnail image_url =
      HH.img
      [ HP.src $ String.replace (Pattern "photos.") (Replacement "photos-thumbnail.") image_url
      , HP.class_ $ H.ClassName "card-img-top"
      ]

    renderSender (Sender { provider, id }) =
      HH.div_
      [
        HH.div
        [ HP.class_ $ H.ClassName "text-primary" ]
        [ HH.text provider ]
      , HH.div
        [ HP.class_ $ H.ClassName "text-primary" ]
        [ HH.text id ]
      ]

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  Initialize next -> do
    eval $ Reload next

  Reload next -> do
    Util.whenNotBusy_ do
      { client } <- H.get
      res <- H.liftAff $ attempt $ Photos.listFirst' client
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
          res <- H.liftAff $ attempt $ Photos.listNext' client last_
          case res of
            Left err -> do
              H.raise $ Failed "DynamoDB scan failed"
            Right { items, lastKey } -> do
              items_ <- H.gets _.items
              H.modify _{ items = items_ <> items, last = lastKey }

    pure next
