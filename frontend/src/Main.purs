module Main where

import Prelude

import Aws.Dynamo (DYNAMO)
import Component.DynamoUI as DynamoUI
import Control.Monad.Eff (Eff)
import Halogen (action)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects (dynamo :: DYNAMO)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI ui unit body
  io.query $ action DynamoUI.Scan
  where
    ui = DynamoUI.ui "agrishot-test-photos"
