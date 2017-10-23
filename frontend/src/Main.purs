module Main where

import Prelude

import Aws.Dynamo (DYNAMO)
import Aws.Dynamo as Dynamo
import Component.DynamoUI as DynamoUI
import Control.Monad.Eff (Eff)
import Halogen (action)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

dynamoAwsConfig :: Dynamo.AwsConfig
dynamoAwsConfig =
  { region: "local"
  , endpoint: "http://localhost:4569"
  , accessKeyId: "xxxx"
  , secretAccessKey: "xxxx"
  }

main :: Eff (HA.HalogenEffects (dynamo :: DYNAMO)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI ui unit body
  io.query $ action DynamoUI.Scan
  where
    ui = DynamoUI.ui dynamoAwsConfig "agrishot-test-photos"
