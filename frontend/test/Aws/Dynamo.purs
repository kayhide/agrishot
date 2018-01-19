module Test.Aws.Dynamo where

import Prelude

import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as Array
import Data.Char.Gen (genAlpha)
import Data.DateTime.Gen (genDateTime)
import Data.Foreign.Class (encode)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.StrMap as StrMap
import Data.String.Gen (genDigitString, genString)
import Data.Traversable (traverse_)
import Model.Photo (Photo(..))
import Model.Photo as Photo
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck', (===))
import Test.QuickCheck.Gen (Gen, randomSample)

type Aff_ eff a = Aff
                  ( console :: CONSOLE
                  , random :: RANDOM
                  , exception :: EXCEPTION
                  , dynamo :: DYNAMO
                  | eff) a

test :: forall eff. Aff_ eff Unit
test = do
  clean
  photos <- liftEff $ randomSample genPhoto
  traverse_ Dynamo.put $ params <$> photos

  n <- countPhotos
  liftEff $ quickCheck' 1 (n === Array.length photos)

  ids <- allIds
  liftEff $ quickCheck' 1 (Array.sort ids === Array.sort ((_.id <<< unwrap) <$> photos))

  clean
  m <- countPhotos
  liftEff $ quickCheck' 1 (m === Array.length photos)

  where
    unwrap' (TestPhoto item) = item
    params item =
      { "TableName": tableName
      , "Item": encode item
      }

tableName :: String
tableName = "agrishot-test-photos"

genPhoto :: Gen Photo
genPhoto = arbitrary >>= case _ of TestPhoto x -> pure x

newtype TestPhoto = TestPhoto Photo

instance arbitraryTestPhoto :: Arbitrary TestPhoto where
  arbitrary = do
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
    pure $ TestPhoto $ Photo { id, sender_id, sender, image_url, created_at }
    where
      genAlphaString = genString genAlpha

clean :: forall eff. Aff_ eff Unit
clean = do
  ids <- allIds
  traverse_ Dynamo.delete $ params <$> ids
  where
    params id =
      { "TableName": tableName
      , "Key": encode $ StrMap.singleton "id" id
      }

allIds :: forall eff. Aff_ eff (Array String)
allIds = map _.id <$> _."Items" <$> Dynamo.scan params
  where
    params = { "TableName": tableName }

countPhotos :: forall eff. Aff_ eff Int
countPhotos = _."Count" <$> Dynamo.scan params
  where
    params = { "TableName": tableName }
