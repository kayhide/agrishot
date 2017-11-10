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

data Query a =
  Initialize a

type Input = State

type Message = Void

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
  [ HH.text $ text state.notice ]

  where
    classes (Info s) = HP.classes [ H.ClassName "alert", H.ClassName "alert-info" ]
    classes (Alert s) = HP.classes [ H.ClassName "alert", H.ClassName "alert-danger" ]

    text (Info s) = s
    text (Alert s) = s

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Aff eff)
eval = case _ of
  Initialize next -> do
    pure next
