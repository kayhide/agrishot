module Aws.Cognito where

import Prelude

import Aws.Config (AwsConfig)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (kind Effect, Eff)


foreign import data COGNITO :: Effect

foreign import setRegion :: forall eff. String -> Eff (cognito :: COGNITO | eff) Unit
foreign import setIdentityPoolId :: forall eff. String -> Eff (cognito :: COGNITO | eff) Unit
foreign import setFacebookToken :: forall eff. String -> Eff (cognito :: COGNITO | eff) Unit

foreign import _authenticate :: forall c eff. (AwsConfig c -> Eff eff Unit) -> Eff eff Unit

authenticate :: forall c eff. Aff (cognito :: COGNITO | eff) (AwsConfig c)
authenticate = makeAff callback
  where
    callback _ onSuccess = _authenticate onSuccess
