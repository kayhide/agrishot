module Test.Aws.Dynamo where

import Prelude

import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Char.Gen (genAlpha)
import Data.DateTime.Gen (genDateTime)
import Data.Either (fromRight)
import Data.Foreign.Class (decode, encode)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.StrMap as StrMap
import Data.String.Gen (genDigitString, genString)
import Data.Traversable (traverse, traverse_)
import Model.Photo (Photo(..))
import Model.Photo as Photo
import Partial.Unsafe (unsafePartial)
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
  traverse_ put_ photos

  n <- count_
  liftEff $ quickCheck' 1 (n === 10)

  ids <- map (_.id <<< unwrap) <$> scan_
  liftEff $ quickCheck' 1 (Array.sort ids === Array.sort ((_.id <<< unwrap) <$> photos))

  let id = unsafePartial $ fromJust $ Array.head ids
  Photo obj <- get_ id
  liftEff $ quickCheck' 1 (obj.id === id)

  delete_ id
  m <- count_
  liftEff $ quickCheck' 1 (m === 9)


tableName :: String
tableName = "agrishot-test-photos"


scan_ :: forall eff. Aff_ eff (Array Photo)
scan_ = do
  res <- Dynamo.scan { "TableName": tableName }
  pure $ unsafePartial $ fromRight $ runExcept $ traverse decode $ res."Items"


put_ :: forall eff. Photo -> Aff_ eff Unit
put_ item =
  Dynamo.put
  { "TableName": tableName
  , "Item": encode item
  }

get_ :: forall eff. String -> Aff_ eff Photo
get_ id = do
  res <- Dynamo.get
         { "TableName": tableName
         , "Key": encode $ StrMap.singleton "id" id
         }
  pure $ unsafePartial $ fromRight $ runExcept $ decode $ res."Item"

delete_ :: forall eff. String -> Aff_ eff Unit
delete_ id =
  Dynamo.delete
  { "TableName": tableName
  , "Key": encode $ StrMap.singleton "id" id
  }

count_ :: forall eff. Aff_ eff Int
count_ = _."Count" <$> Dynamo.scan { "TableName": tableName }


clean :: forall eff. Aff_ eff Unit
clean = do
  ids <- map (_.id <<< unwrap) <$> scan_
  traverse_ delete_ ids

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
