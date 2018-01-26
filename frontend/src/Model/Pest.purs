module Model.Pest where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))

type PestId = String

type PestRec =
  { id :: PestId
  , label :: String
  , description :: String
  , url :: String
  }
newtype Pest = Pest PestRec

_Pest :: Lens' Pest PestRec
_Pest = lens (\(Pest r) -> r) (\_ r -> Pest r)

_id :: Lens' Pest PestId
_id = _Pest <<< prop (SProxy :: SProxy "id")

_label :: Lens' Pest String
_label = _Pest <<< prop (SProxy :: SProxy "label")

_description :: Lens' Pest String
_description = _Pest <<< prop (SProxy :: SProxy "description")

_url :: Lens' Pest String
_url = _Pest <<< prop (SProxy :: SProxy "url")


derive instance newtypePest :: Newtype Pest _
derive instance genericPest :: Generic Pest _
derive instance eqPest :: Eq Pest

instance showPest :: Show Pest where
  show = genericShow

instance encodePest :: Encode Pest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

instance decodePest :: Decode Pest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
