module Component.DynamoUI where

import Prelude

import Aws.Dynamo (DYNAMO, ScanResult, scan)
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

newtype Photo =
  Photo
  { id :: String
  , image_url :: String
  , created_at :: Number
  }

type State = Array Photo

data Query a = Scan a


type TableName = String

ui :: forall eff.
      TableName ->
      H.Component HH.HTML Query Unit Void (Aff (dynamo :: DYNAMO | eff))
ui tableName =
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
        , HH.div_ [ HH.text (show created_at) ]
        ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dynamo :: DYNAMO | eff))
  eval (Scan next) = do
    res <- H.liftAff $ scan conf opts
    H.put (res :: ScanResult Photo)."Items"
    pure next
    where
      conf = { region: "local", endpoint: "http://localhost:4569", accessKeyId: "xxxx", secretAccessKey: "xxxx" }
      opts = { "TableName": tableName }
