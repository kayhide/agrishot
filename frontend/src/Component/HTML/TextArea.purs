module Component.HTML.TextArea where

import Prelude

import Halogen (Action)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall q p. String -> String -> String -> (String -> Action q) -> HH.HTML p (q Unit)
render key label value query =
  HH.div
  [ HP.class_ $ H.ClassName "form-group" ]
  [
    HH.label
    [ HP.for key ]
    [ HH.text label ]
  , HH.textarea
    [ HP.class_ $ H.ClassName "form-control"
    , HP.id_ key
    , HP.value value
    , HE.onValueInput $ HE.input query
    ]
  ]

renderWithHelp :: forall q p. String -> String -> String -> String -> (String -> Action q) -> HH.HTML p (q Unit)
renderWithHelp key label value help query =
  HH.div
  [ HP.class_ $ H.ClassName "form-group" ]
  [
    HH.label
    [ HP.for key ]
    [ HH.text label ]
  , HH.input
    [ HP.class_ $ H.ClassName "form-control"
    , HP.id_ key
    , HP.value value
    , HE.onValueInput $ HE.input query
    ]
  , HH.small
    [ HP.class_ $ H.ClassName "form-text text-muted" ]
    [ HH.text help ]
  ]
