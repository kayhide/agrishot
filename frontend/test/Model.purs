module Test.Model where

import Prelude

import Data.Char.Gen (genAlpha)
import Data.DateTime.Gen (genDateTime)
import Data.Maybe (Maybe(..))
import Data.String.Gen (genDigitString, genString)
import Model.Photo (Photo(..))
import Model.Photo as Photo
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen)


genPhoto :: Gen Photo
genPhoto = do
  id <- genDigitString
  sender_id <- genAlphaString
  sender <- Just <$> do
    r <- { id: _, provider: _ } <$> genAlphaString <*> genAlphaString
    pure $ Photo.Sender r
  image_url <- do
    domain <- genAlphaString
    basename <- genAlphaString
    pure $ "http://" <> domain <> ".test/" <> basename <> ".jpg"
  created_at <- genDateTime
  pure $ Photo { id, sender_id, sender, image_url, created_at, part: 0 }
  where
    genAlphaString = genString genAlpha


newtype TestPhoto = TestPhoto Photo
instance arbitraryTestPhoto :: Arbitrary TestPhoto where
  arbitrary = TestPhoto <$> genPhoto
