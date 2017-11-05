module Dom.Meta where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (Foreign, readString)
import Data.Maybe (Maybe(..))

foreign import data META :: Effect

foreign import _get :: String -> Foreign

get :: forall eff. String -> Eff (meta :: META | eff) (Maybe String)
get name = case res of
  Right val -> pure $ Just val
  Left _ -> pure $ Nothing
  where
    res = runExcept do
      readString $ _get name
