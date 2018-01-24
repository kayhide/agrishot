module Component.Route where

import Prelude

import Control.Alt ((<|>))
import Routing.Match (Match)
import Routing.Match.Class (lit)

data Location
  = Home
  | PhotoListPage
  | PestListPage

routing :: Match Location
routing = photos <|> pests <|> home
  where
    home = Home <$ lit ""
    photos = PhotoListPage <$ route "photos"
    pests = PestListPage <$ route "pests"

    route str = lit "" *> lit str


path :: Location -> String
path = case _ of
  Home -> "#/"
  PhotoListPage -> "#/photos"
  PestListPage -> "#/pests"
