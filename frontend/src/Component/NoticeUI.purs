module Component.NoticeUI where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


data Notice = Info String | Alert String

type State =
  { id :: Int
  , notice :: Notice
  }

data Query a
  = Initialize a
  | Close a

type Input = State

data Message =
  Closed Int

ui :: forall eff. H.Component HH.HTML Query Input Message (Aff eff)
ui =
  H.lifecycleComponent
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div
  [ classes state.notice ]
  [
    HH.text $ text state.notice
  , HH.a
    [ HE.onClick $ HE.input_ Close
    , HP.href "#"
    , HP.class_ $ H.ClassName "pull-right alert-link"
    ]
    [ HH.i
      [ HP.class_ $ H.ClassName "fa fa-times" ]
      []
    ]
  ]

  where
    classes (Info s) = HP.class_ $ H.ClassName "alert alert-info"
    classes (Alert s) = HP.class_ $ H.ClassName "alert alert-danger"

    text (Info s) = s
    text (Alert s) = s

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Aff eff)
eval = case _ of
  Initialize next -> do
    pure next

  Close next -> do
    i <- H.gets _.id
    H.raise $ Closed i
    pure next
