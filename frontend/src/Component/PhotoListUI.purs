module Component.PhotoListUI where

import Prelude

import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Aws.Dynamo.Query as DQ
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.DateTime as DateTime
import Data.DateTime.Locale (Locale(Locale))
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Foreign (Foreign)
import Data.Foreign.Class (decode)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Model.Photo (Photo(..), Sender(..))


type TableName = String
type IndexName = String

data Query a
  = SetLocale Locale a
  | Scan a
  | GetState (State -> a)

type State =
  { tableName :: TableName
  , indexNamePartCreatedAt :: IndexName
  , items :: Array Photo
  , alerts :: Array String
  , busy :: Boolean
  , locale :: Locale
  }

type Input =
  { tableName :: TableName
  , indexNamePartCreatedAt :: IndexName
  , locale :: Locale
  }

data Message
  = Scanned (Array Photo)
  | Failed String


type Eff_ eff = Aff (dynamo :: DYNAMO | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: Input -> State
initialState { tableName, indexNamePartCreatedAt, locale } =
    { tableName: tableName
    , indexNamePartCreatedAt: indexNamePartCreatedAt
    , items: []
    , alerts: []
    , busy: false
    , locale: locale
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
  [ HH.div
    [ HP.class_ $ H.ClassName "row" ]
    (renderItem <$> state.items)
  , HH.div_ (renderAlert <$> state.alerts)
  , renderBusy state.busy
  ]

  where
    renderAlert s =
      HH.pre_
      [ HH.text s ]

    renderBusy false = HH.p_ []
    renderBusy true =
      HH.p
      [ HP.class_ $ H.ClassName "text-center" ]
      [
        HH.i [ HP.class_ $ H.ClassName "fa fa-spinner fa-pulse fa-3x" ] []
      ]

    renderItem (Photo { id, sender_id, image_url, created_at, sender }) =
      HH.div
      [ HP.classes [ H.ClassName "col-md-2", H.ClassName "col-sm-6", H.ClassName "col-xs-12" ] ]
      [ HH.div
        [ HP.classes [ H.ClassName "card", H.ClassName "mb-2" ] ]
        [
          HH.a
          [ HP.href image_url, HP.target "_blank" ]
          [
            HH.img [ HP.src thumbnail_url, HP.class_ $ H.ClassName "card-img-top" ]
          ]
        , HH.div
          [ HP.classes [ H.ClassName "card-body" ] ]
          [ HH.p
            [ HP.classes [ H.ClassName "card-text small" ] ]
            [
              HH.i
              [ HP.class_ $ H.ClassName "fa fa-user"
              , HP.title sender_id
              ] []
            , renderSender sender
            ]
          , HH.p
            [ HP.classes [ H.ClassName "card-text", H.ClassName "text-muted", H.ClassName "small" ] ]
            [ renderDateTime created_at state.locale ]
          ]
        ]
      ]
      where
        thumbnail_url = String.replace (Pattern "photos.") (Replacement "photos-thumbnail.") image_url

    renderSender (Just (Sender { provider, id })) =
      HH.div_
      [
        HH.div
        [ HP.class_ $ H.ClassName "text-primary" ]
        [ HH.text provider ]
      , HH.div
        [ HP.class_ $ H.ClassName "text-primary" ]
        [ HH.text id ]
      ]
    renderSender Nothing = HH.div_ []

    renderDateTime dt (Locale _ dur) =
      HH.text $ either id id $ maybe (Left "") (formatDateTime "YYYY/MM/DD HH:mm:ss") dt_
      where
        dt_ = (DateTime.adjust (negate dur)) dt

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval = case _ of
  SetLocale locale next -> do
    H.modify _{ locale = locale }
    pure next

  Scan next -> do
    busy <- H.gets _.busy
    when (not busy) do
      H.modify _{ busy = true }
      tableName <- H.gets _.tableName
      indexName <- H.gets _.indexNamePartCreatedAt
      items <- H.liftAff $ attempt $ do
        items <- _."Items" <$> Dynamo.query do
          DQ.tableName tableName
          DQ.indexName indexName
          DQ.descending
          DQ.keyCondition $ DQ.eq_ "#part" 0
        getItems items

      case items of
        Left err -> do
          H.modify _{ items = [], alerts = [show err], busy = false }
          H.raise $ Failed "DynamoDB scan failed"

        Right items_ -> do
          H.modify _{ items = items_, alerts = [], busy = false }
          H.raise $ Scanned items_

    pure next

  GetState reply -> do
    state <- H.get
    pure $ reply state


getItems :: forall eff. Array Foreign -> Aff eff (Array Photo)
getItems objs =
  case runExcept (traverse decode objs) of
    Right xs -> pure xs
    Left err -> do
      throwError $ (error <<< intercalate "\n\n" <<< map show) err
