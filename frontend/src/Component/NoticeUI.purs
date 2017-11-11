module Component.NoticeUI where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds(..), delay)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


data Notice = Info String | Alert String

type IdNotice =
  { id :: Int
  , notice :: Notice
  }

type State =
  { entity :: IdNotice
  , animated :: Maybe String
  , pinned :: Boolean
  , pinCount :: Int
  }

data Query a
  = Initialize a
  | Pin a
  | WaitAndClose Milliseconds a
  | Close a

type Input = IdNotice

data Message =
  Closed Int

ui :: forall eff. H.Component HH.HTML Query Input Message (Aff eff)
ui =
  H.lifecycleComponent
    { initialState: { entity: _, animated: Nothing, pinned: false, pinCount: 0 }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

render :: State -> H.ComponentHTML Query
render state =
  HH.div
  [ HP.classes $ H.ClassName <$> Array.catMaybes [ state.animated ] ]
  [
    HH.div
    [ classes state.entity.notice ]
    [
      HH.div
      [ HP.class_ $ H.ClassName "position-relative" ]
      [
        HH.text $ text state.entity.notice
      , HH.a
        [ HE.onClick $ HE.input_ Pin
        , HP.href "#"
        , HP.class_ $ H.ClassName "position-absolute-right position-absolute-top alert-link"
        ]
        [
          renderPinIcon state.pinned
        ]
      ]
    ]
  ]

  where
    classes (Info s) = HP.class_ $ H.ClassName "alert alert-info"
    classes (Alert s) = HP.class_ $ H.ClassName "alert alert-danger"

    text (Info s) = s
    text (Alert s) = s

    renderPinIcon false =
      HH.i [ HP.class_ $ H.ClassName "notice-pin fa fa-map-pin fa-rotate-90" ] []
    renderPinIcon true =
      HH.i [ HP.class_ $ H.ClassName "notice-pin notice-pin-on fa fa-map-pin" ] []

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Aff eff)
eval = case _ of
  Initialize next -> do
    H.modify _{ animated = Just "notice notice-out" }
    H.liftAff $ delay (Milliseconds 0.0)
    H.modify _{ animated = Just "notice notice-in" }
    pinned <- H.gets _.pinned
    if pinned
      then pure next
      else eval $ WaitAndClose (Milliseconds 3000.0) next

  Pin next -> do
    pinned <- not <$> H.gets _.pinned
    pinCount <- (_ + 1) <$> H.gets _.pinCount
    H.modify _{ pinned = pinned, pinCount = pinCount }
    if pinned
      then pure next
      else eval $ WaitAndClose (Milliseconds 1000.0) next

  WaitAndClose ms next -> do
    pinCount <- H.gets _.pinCount
    H.liftAff $ delay ms
    pinCount_ <- H.gets _.pinCount
    if pinCount_ == pinCount
      then eval $ Close next
      else pure next

  Close next -> do
    i <- H.gets _.entity.id
    H.modify _{ animated = Just "notice notice-out" }
    H.liftAff $ delay (Milliseconds 500.0)
    H.raise $ Closed i
    pure next
