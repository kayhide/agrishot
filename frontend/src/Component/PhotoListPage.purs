module Component.PhotoListPage where

import Prelude

import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Aws.Dynamo.Query as DQ
import Component.HTML.DateTime as DateTime
import Component.HTML.LoadingIndicator as LoadingIndicator
import Component.Util as Util
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Array as Array
import Data.DateTime.Locale (Locale)
import Data.Either (Either(Left, Right))
import Data.Foldable (intercalate)
import Data.Foreign (Foreign)
import Data.Foreign.Class (decode)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model.Photo (Photo(..), Sender(..))


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


type TableName = String
type IndexName = String

data Query a
  = Initialize a
  | SetLocale Locale a
  | Reload a

type State =
  { tableName :: TableName
  , indexNamePartCreatedAt :: IndexName
  , items :: Array Photo
  , busy :: Boolean
  , locale :: Locale
  }

type Input =
  { tableName :: TableName
  , indexNamePartCreatedAt :: IndexName
  , locale :: Locale
  }

data Message
  = Failed String


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
initialState { tableName, indexNamePartCreatedAt, locale } =
    { tableName: tableName
    , indexNamePartCreatedAt: indexNamePartCreatedAt
    , items: []
    , busy: false
    , locale: locale
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
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

  where
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

  SetLocale locale next -> do
    H.modify _{ locale = locale }
    pure next

  Reload next -> do
    Util.whenNotBusy_ do
      { tableName, indexNamePartCreatedAt } <- H.get
      items <- H.liftAff $ attempt $
        getItems =<< _."Items" <$> Dynamo.query do
          DQ.tableName tableName
          DQ.indexName indexNamePartCreatedAt
          DQ.descending
          DQ.keyCondition $ DQ.eq_ "#part" 0

      case items of
        Left err -> do
          H.raise $ Failed "DynamoDB scan failed"

        Right items_ -> do
          H.modify _{ items = items_ }

    pure next


getItems :: forall eff. Array Foreign -> Aff eff (Array Photo)
getItems objs =
  case runExcept (traverse decode objs) of
    Right xs -> pure xs
    Left err -> do
      throwError $ (error <<< intercalate "\n\n" <<< map show) err
