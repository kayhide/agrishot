module Aws.Config where

import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign)

newtype AwsConfig = AwsConfig Foreign
newtype AwsCredentials = AwsCredentials Foreign

foreign import build :: forall r eff. { region :: Region | r } -> Eff eff AwsConfig

foreign import readCredentials :: forall eff. ProfileName -> Eff eff AwsCredentials

type Region = String
type ProfileName = String
