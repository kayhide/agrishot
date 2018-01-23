module Test.Aws.Dynamo where

import Prelude

import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Aws.Dynamo.Query as DQ
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (fromRight)
import Data.Foreign (readArray)
import Data.Foreign.Class (decode, encode)
import Data.Foreign.Index ((!))
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.StrMap as StrMap
import Data.Traversable (traverse, traverse_)
import Model.Photo (Photo(..))
import Partial.Unsafe (unsafePartial)
import Test.Model (genPhoto)
import Test.QuickCheck (quickCheck', (===))
import Test.QuickCheck.Gen (randomSample)

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
  res <- Dynamo.scan do
    DQ.tableName tableName
  pure $ unsafePartial $ fromRight $ runExcept $ do
    traverse decode =<< readArray =<< res ! "Items"


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
count_ = do
  res <- Dynamo.scan do
    DQ.tableName tableName
  pure $ unsafePartial $ fromRight $ runExcept $ decode =<< res ! "Count"


clean :: forall eff. Aff_ eff Unit
clean = do
  ids <- map (_.id <<< unwrap) <$> scan_
  traverse_ delete_ ids
