module Component.PhotoListUI where

import Prelude

import Aws.Dynamo (DYNAMO, scan)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foreign.Class (decode)
import Data.Formatter.DateTime (formatDateTime)
import Data.List.NonEmpty as NEL
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
  where

  initialState :: Input -> State
  initialState =
    { tableName: _
    , items: []
    , message: "Initializing..."
    }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.div_ (renderItem <$> state.items)
      ]

    where
      renderList { items } =
        HH.div_ (renderItem <$> items)

      renderItem (Photo { id, image_url, created_at }) =
        HH.div_
        [ HH.div_
          [ HH.img [ HP.src image_url ] ]
        , HH.div_ [ HH.text id ]
        , HH.div_ [ renderDateTime created_at ]
        ]

      renderDateTime dt =
        HH.text $ either id id $ formatDateTime "YYYY/MM/DD hh:mm:ss" dt

  eval :: Query ~> H.ComponentDSL State Query Message (Aff (dynamo :: DYNAMO | eff))
  eval (Scan next) = do
    tableName <- H.gets _.tableName
    let opts = { "TableName": tableName }
    res <- H.liftAff $ scan opts
    photos <- H.liftAff $ getItems res."Items"
    H.modify (_ { items = photos })
    H.raise $ Scanned photos
    pure next

    where
      getItems objs =
        case runExcept (traverse decode objs) of
          Right xs -> pure xs
          Left err -> throwError $ error $ show $ NEL.head err

  eval (HandleInput tableName next) = do
    old <- H.gets _.tableName
    when (old /= tableName) do
      H.modify _{ tableName = tableName }
      H.modify _{ message = "Input received." }
      -- H.action Scan
    pure next

  eval (GetState reply) = do
    state <- H.get
    pure $ reply state
