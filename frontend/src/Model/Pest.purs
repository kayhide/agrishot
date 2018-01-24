module Model.Pest where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)


type PestRec =
  { id :: String
  , label :: String
  , description :: String
  , url :: String
  }
newtype Pest = Pest PestRec

derive instance newtypePest :: Newtype Pest _
derive instance genericPest :: Generic Pest _
instance showPest :: Show Pest where
  show = genericShow

instance encodePest :: Encode Pest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

instance decodePest :: Decode Pest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
