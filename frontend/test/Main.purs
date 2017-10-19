module Test.Main where

import Prelude (class Show, bind, discard, pure, show, ($), (<<<))
import Control.Monad.Aff (Canceler, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, errorShow)
import Data.Generic.Rep(class Generic)
import Data.Generic.Rep.Show(genericShow)
import Data.Traversable (traverse_)

import Aws.Dynamo

main :: Eff (console :: CONSOLE, dynamo :: DYNAMO) (Canceler (console :: CONSOLE, dynamo :: DYNAMO))
main =
  runAff errorShow pure do
    liftEff $ log "OK"
    res <- scan conf opts
    liftEff do
      log "OK*********"
      log "success:"
      traverse_ (log <<< show) $ (res :: ScanResult Photo)."Items"
  where
    conf = { region: "local", endpoint: "http://localhost:4569" }
    opts = { "TableName": "agrishot-test-photos" }

newtype Photo =
  Photo
  { id :: String
  , image_url :: String
  , created_at :: Number
  }

derive instance genericPhoto :: Generic Photo _
instance showPhoto :: Show Photo where
  show = genericShow
