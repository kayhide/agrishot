module Test.Api.Photos where

import Prelude

import Api as Api
import Api.Photos as Photos
import Aws.Dynamo (DYNAMO)
import Aws.Dynamo.Query as DQ
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as Array
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
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
  clean cli
  photos <- liftEff $ randomSample genPhoto
  traverse_ (Photos.create cli) photos

  n <- Photos.count cli
  liftEff $ quickCheck' 1 (n === 10)

  res1 <- Photos.listFirst cli do
    DQ.limit 4
  liftEff $ quickCheck' 1 (isJust res1.lastKey)

  res2 <- Photos.listNext cli (unsafePartial $ fromJust res1.lastKey) do
    DQ.limit 4
  liftEff $ quickCheck' 1 (isJust res2.lastKey)

  res3 <- Photos.listNext cli (unsafePartial $ fromJust res2.lastKey) do
    DQ.limit 4
  liftEff $ quickCheck' 1 (isNothing res3.lastKey)

  let ids = _.id <<< unwrap <$> Array.concat [res1.items, res2.items, res3.items]
  liftEff $ quickCheck' 1 (Array.sort ids === Array.sort ((_.id <<< unwrap) <$> photos))

  let id = unsafePartial $ fromJust $ Array.head ids
  photo@(Photo obj) <- Photos.find cli id
  liftEff $ quickCheck' 1 (obj.id === id)

  Photos.destroy cli photo
  m <- Photos.count cli
  liftEff $ quickCheck' 1 (m === 9)

  where
    cli = Api.makeClient "test"


clean :: forall eff. Api.Client -> Aff_ eff Unit
clean cli = do
  res <- Photos.listFirst' cli
  traverse_ (Photos.destroy cli) res.items
