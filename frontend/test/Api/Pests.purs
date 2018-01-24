module Test.Api.Pests where

import Prelude

import Api as Api
import Api.Pests as Pests
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
import Model.Pest (Pest(..))
import Partial.Unsafe (unsafePartial)
import Test.Model (genPest)
import Test.QuickCheck (quickCheck', (===))
import Test.QuickCheck.Gen (randomSample)

type Aff_ eff a =
  Aff
  ( console :: CONSOLE
  , random :: RANDOM
  , exception :: EXCEPTION
  , dynamo :: DYNAMO
  | eff) a


test :: forall eff. Aff_ eff Unit
test = do
  clean cli
  pests <- liftEff $ randomSample genPest
  traverse_ (Pests.create cli) pests

  n <- Pests.count cli
  liftEff $ quickCheck' 1 (n === 10)

  res <- Pests.listFirst' cli

  let ids = _.id <<< unwrap <$> res.items
  liftEff $ quickCheck' 1 (Array.sort ids === Array.sort ((_.id <<< unwrap) <$> pests))

  let id = unsafePartial $ fromJust $ Array.head ids
  photo@(Pest obj) <- Pests.find cli id
  liftEff $ quickCheck' 1 (obj.id === id)

  Pests.destroy cli photo
  m <- Pests.count cli
  liftEff $ quickCheck' 1 (m === 9)

  where
    cli = Api.makeClient "test"


clean :: forall eff. Api.Client -> Aff_ eff Unit
clean cli = do
  res <- Pests.listFirst' cli
  traverse_ (Pests.destroy cli) res.items
