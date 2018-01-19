module Api where

import Prelude


newtype Client =
  Client
  { photos ::
    { tableName :: String
    , indexNamePartCreatedAt :: String
    }
  }

makeClient :: String -> Client
makeClient stage =
  Client
  { photos:
    { tableName: "agrishot-" <> stage <> "-photos"
    , indexNamePartCreatedAt: "agrishot-" <> stage <> "-photos-part-created_at"
    }
  }
