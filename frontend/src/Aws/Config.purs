module Aws.Config where

import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign)

newtype AwsConfig = AwsConfig Foreign

foreign import build :: forall r eff. { region :: String | r } -> Eff eff AwsConfig
