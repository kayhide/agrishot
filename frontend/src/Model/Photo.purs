module Model.Photo where

import Control.Monad (map, (=<<))
import Control.Monad.Except (mapExcept)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(..), readNumber, readString, tagOf)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty as NEL
import Data.Maybe (maybe)
import Data.Time.Duration (Milliseconds(..))
import Prelude (class Show, bind, const, pure, ($), (<<<))


newtype Photo =
  Photo
  { id :: String
  , image_url :: String
  , created_at :: DateTime
  }

derive instance genericPhoto :: Generic Photo _
instance showPhoto :: Show Photo where
  show = genericShow

instance decodePhoto :: Decode Photo where
  decode v = do
    id <- readString =<< v ! "id"
    image_url <- readString =<< v ! "image_url"
    created_at <- readDateTime =<< v ! "created_at"
    pure $ Photo { id, image_url, created_at }

readDateTime :: Foreign -> F DateTime
readDateTime value = mapExcept (either (const error) fromNumber) (readNumber value)
  where
    fromNumber = maybe error pure <<< map toDateTime <<< instant <<< Milliseconds
    error = Left $ NEL.singleton $ TypeMismatch "DateTime" (tagOf value)
