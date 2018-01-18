module Component.Route where

import Prelude

import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit)

data Location
  = Home
  | PhotoListPage

routing :: Match Location
routing = photoListPage <|> home
  where
    home = Home <$ lit ""
    photoListPage = PhotoListPage <$ route "photos"

    route str = lit "" *> lit str


path :: Location -> String
path = case _ of
  Home -> "#/"
  PhotoListPage -> "#/photos"
