module Component.PhotoListUI where

import Prelude

import Aws.Dynamo (DYNAMO, scan)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Foreign.Class (decode)
import Data.Formatter.DateTime (formatDateTime)
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model.Photo (Photo(..))


data Query a
  = Scan a
  | HandleInput TableName a
  | GetState (State -> a)

type State =
  { tableName :: TableName
  , items :: Array Photo
  , message :: String
  , alerts :: Array String
  }

type Input = TableName

data Message = Scanned (Array Photo)

type TableName = String

ui :: forall eff.
      H.Component HH.HTML Query Input Message (Aff (dynamo :: DYNAMO | eff))
ui =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }

initialState :: Input -> State
initialState =
    { tableName: _
    , items: []
    , message: "Initializing..."
    , alerts: []
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
  [ HH.div
    [ HP.class_ $ H.ClassName "row" ]
    (renderItem <$> state.items)
  , HH.div_ (renderAlert <$> state.alerts)
  ]

  where
    renderAlert s =
      HH.pre_
      [ HH.text s ]

    renderItem (Photo { id, image_url, created_at }) =
      HH.div
      [ HP.classes [ H.ClassName "col-md-2", H.ClassName "col-sm-6", H.ClassName "col-xs-12" ] ]
      [ HH.div
        [ HP.classes [ H.ClassName "card", H.ClassName "mb-2" ] ]
        [ HH.img [ HP.src image_url, HP.class_ $ H.ClassName "card-img-top" ]
        , HH.div
          [ HP.classes [ H.ClassName "card-body" ] ]
          [ HH.p
            [ HP.classes [ H.ClassName "card-text" ] ]
            [ HH.text id ]
          , HH.p
            [ HP.classes [ H.ClassName "card-text", H.ClassName "text-muted", H.ClassName "small" ] ]
            [ renderDateTime created_at ]
          ]
        ]
      ]

    renderDateTime dt =
      HH.text $ either id id $ formatDateTime "YYYY/MM/DD hh:mm:ss" dt

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Aff (dynamo :: DYNAMO | eff))
eval (Scan next) = do
  opts <- { "TableName": _ } <$> H.gets _.tableName
  items <- H.liftAff $ attempt $ do
    getItems <<< _."Items" =<< scan opts

  case items of
    Left err ->
      H.modify _{ items = [], alerts = [show err] }

    Right items_ -> do
      H.modify _{ items = items_, alerts = [] }
      H.raise $ Scanned items_

  pure next

  where
    getItems objs =
      case runExcept (traverse decode objs) of
        Right xs -> pure xs
        Left err -> do
          throwError $ (error <<< intercalate "\n\n" <<< map show) err

eval (HandleInput tableName next) = do
  old <- H.gets _.tableName
  if (old /= tableName)
    then do
    H.modify _{ tableName = tableName }
    H.modify _{ message = "Input received." }
    eval (Scan next)
    else
    pure next

eval (GetState reply) = do
  state <- H.get
  pure $ reply state
