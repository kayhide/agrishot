module Component.HTML.DateTime where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.DateTime.Locale (Locale(..))
import Data.Either (hush)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (fromMaybe)
import Halogen.HTML as HH


render :: forall p i. DateTime -> Locale -> HH.HTML p i
render dt (Locale _ dur) =
  HH.text $ fromMaybe "" $ do
    dt_ <- DateTime.adjust (negate dur) dt
    hush $ formatDateTime "YYYY/MM/DD HH:mm:ss" dt_
