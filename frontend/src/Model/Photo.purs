module Model.Photo where

import Prelude

import Control.Monad.Except (mapExcept)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(TypeMismatch), readNullOrUndefined, readNumber, tagOf)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe, maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)


newtype Photo =
  Photo
  { id :: String
  , sender_id :: String
  , sender :: Maybe Sender
  , image_url :: String
  , created_at :: DateTime
  }

newtype Sender =
  Sender
  { id :: String
  , provider :: String
  }

derive instance genericPhoto :: Generic Photo _
instance showPhoto :: Show Photo where
  show = genericShow

instance decodePhoto :: Decode Photo where
  decode v = do
    id <- decode =<< v ! "id"
    sender_id <- decode =<< v ! "sender_id"
    sender <- traverse decode =<< readNullOrUndefined =<< v ! "sender"
    image_url <- decode =<< v ! "image_url"
    created_at <- readDateTime =<< v ! "created_at"
    pure $ Photo { id, sender_id, sender: sender, image_url, created_at }

derive instance genericSender :: Generic Sender _
instance showSender :: Show Sender where
  show = genericShow

instance decodeSender :: Decode Sender where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

readDateTime :: Foreign -> F DateTime
readDateTime value = mapExcept (either (const error) fromNumber) (readNumber value)
  where
    fromNumber = maybe error pure <<< map toDateTime <<< instant <<< Milliseconds
    error = Left $ NEL.singleton $ TypeMismatch "DateTime" (tagOf value)
