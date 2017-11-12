module Aws.Cognito where

import Prelude

import Aws.Config (AwsConfig)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (kind Effect, Eff)


foreign import data COGNITO :: Effect

foreign import setRegion :: forall eff. String -> Eff (cognito :: COGNITO | eff) Unit
foreign import setIdentityPoolId :: forall eff. String -> Eff (cognito :: COGNITO | eff) Unit
foreign import setFacebookToken :: forall eff. String -> Eff (cognito :: COGNITO | eff) Unit

foreign import _authenticate :: forall eff. EffFnAff (cognito :: COGNITO | eff) AwsConfig

authenticate :: forall eff. Aff (cognito :: COGNITO | eff) AwsConfig
authenticate = fromEffFnAff _authenticate
