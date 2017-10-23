module Component.DynamoUI where

import Prelude

import Aws.Dynamo (DYNAMO, AwsConfig, scan)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Data.Array as Array
import Data.DateTime (DateTime(..))
import Data.Either (Either(..), either)
import Data.Foreign (Foreign)
import Data.Foreign.Class (decode)
import Data.Formatter.DateTime (formatDateTime)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model.Photo (Photo(..))


type State = Array Photo

data Query a = Scan a


type TableName = String

ui :: forall eff.
      AwsConfig ->
      TableName ->
      H.Component HH.HTML Query Unit Void (Aff (dynamo :: DYNAMO | eff))
ui awsConfig tableName =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = []

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Photo List" ]
      , HH.div_
          [ HH.text (show (Array.length state)) ]
      , HH.div_ (renderItem <$> state)
      , HH.button
          [ HE.onClick (HE.input_ Scan) ]
          [ HH.text "Reload" ]
      ]
    where
      renderItem (Photo { id, image_url, created_at }) =
        HH.div_
        [ HH.div_
          [ HH.img [ HP.src image_url ] ]
        , HH.div_ [ HH.text id ]
        , HH.div_ [ renderDateTime created_at ]
        ]

      renderDateTime dt =
        HH.text $ either id id $ formatDateTime "YYYY/MM/DD hh:mm:ss" dt

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dynamo :: DYNAMO | eff))
  eval (Scan next) = do
    res <- H.liftAff $ scan awsConfig opts
    photos <- H.liftAff $ getItems res."Items"
    H.put photos
    pure next
    where
      opts = { "TableName": tableName }

      getItems :: forall eff. Array Foreign -> Aff eff (Array Photo)
      getItems objs =
        case runExcept (traverse decode objs) of
          Right xs -> pure xs
          Left err -> throwError $ error $ show $ NEL.head err
