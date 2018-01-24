module Test.Model where

import Prelude

import Data.Char.Gen (genAlpha)
import Data.DateTime.Gen (genDateTime)
import Data.Maybe (Maybe(..))
import Data.String.Gen (genDigitString, genString)
import Model.Pest (Pest(..))
import Model.Photo (Photo(..))
import Model.Photo as Photo
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen)


genAlphaString :: Gen String
genAlphaString = genString genAlpha

genUrlString :: String -> Gen String
genUrlString suffix = do
  domain <- genAlphaString
  basename <- genAlphaString
  pure $ "http://" <> domain <> ".test/" <> basename <> suffix


genPhoto :: Gen Photo
genPhoto = do
  id <- genDigitString
  sender_id <- genAlphaString
  sender <- Just <$> do
    r <- { id: _, provider: _ } <$> genAlphaString <*> genAlphaString
    pure $ Photo.Sender r
  image_url <- genUrlString ".jpg"
  created_at <- genDateTime
  pure $ Photo { id, sender_id, sender, image_url, created_at, part: 0 }

newtype TestPhoto = TestPhoto Photo
instance arbitraryTestPhoto :: Arbitrary TestPhoto where
  arbitrary = TestPhoto <$> genPhoto


genPest :: Gen Pest
genPest = do
  id <- genDigitString
  label <- genAlphaString
  description <- genAlphaString
  url <- genUrlString "/"
  pure $ Pest { id, label, description, url }

newtype TestPest = TestPest Pest
instance arbitraryTestPest :: Arbitrary TestPest where
  arbitrary = TestPest <$> genPest
