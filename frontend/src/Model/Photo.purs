module Model.Photo where

import Prelude

import Control.Monad.Except (mapExcept)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, instant, toDateTime, unInstant)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(TypeMismatch), readNullOrUndefined, readNumber, tagOf)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Index ((!))
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap as StrMap
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))


newtype Photo =
  Photo
  { id :: String
  , sender_id :: String
  , sender :: Maybe Sender
  , image_url :: String
  , created_at :: DateTime
  , part :: Int
  }

newtype Sender =
  Sender
  { id :: String
  , provider :: String
  }

derive instance newtypePhoto :: Newtype Photo _
derive instance genericPhoto :: Generic Photo _
instance showPhoto :: Show Photo where
  show = genericShow

instance encodePhoto :: Encode Photo where
  encode (Photo photo) =
    encode $ StrMap.fromFoldable
    [ Tuple "id" $ encode photo.id
    , Tuple "sender_id" $ encode photo.sender_id
    , Tuple "sender" $ encode $ NullOrUndefined photo.sender
    , Tuple "image_url" $ encode photo.image_url
    , Tuple "created_at" $ encode $ unwrap $ unInstant $ fromDateTime photo.created_at
    , Tuple "part" $ encode photo.part
    ]

instance decodePhoto :: Decode Photo where
  decode v = do
    id <- decode =<< v ! "id"
    sender_id <- decode =<< v ! "sender_id"
    sender <- traverse decode =<< readNullOrUndefined =<< v ! "sender"
    image_url <- decode =<< v ! "image_url"
    created_at <- readDateTime =<< v ! "created_at"
    part <- decode =<< v ! "part"
    pure $ Photo { id, sender_id, sender, image_url, created_at, part }

derive instance newtypeSender :: Newtype Sender _
derive instance genericSender :: Generic Sender _
instance showSender :: Show Sender where
  show = genericShow

instance encodeSender :: Encode Sender where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeSender :: Decode Sender where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

readDateTime :: Foreign -> F DateTime
readDateTime value = mapExcept (either (const error) fromNumber) (readNumber value)
  where
    fromNumber = maybe error pure <<< map toDateTime <<< instant <<< Milliseconds
    error = Left $ NEL.singleton $ TypeMismatch "DateTime" (tagOf value)
