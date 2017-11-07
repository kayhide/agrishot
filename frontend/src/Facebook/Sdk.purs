module Facebook.Sdk
  ( defaultConfig
  , Config (..)
  , Status (..)
  , Field (..)
  , StatusInfo (..)
  , AuthResponse (..)
  , UserId (..)
  , UserName (..)
  , UserEmail (..)
  , UserInfo (..)
  , AccessToken (..)
  , Sdk
  , AppId
  , ApiPath
  , ApiMethod (..)
  , LoginOptions (..)
  , Scope (..)
  , init
  , loginStatus
  , login
  , logout
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Array (zip)
import Data.Either (Either(Left, Right), either)
import Data.Foreign (F, Foreign, MultipleErrors, readInt, readNullOrUndefined, readString)
import Data.Foreign.Class (class Encode, encode)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap as SM
import Data.String (joinWith)
import Data.Traversable (intercalate, traverse)
import Data.Tuple (Tuple(..))

type AppId = String

newtype Config = Config
  { appId                :: AppId
  , version              :: String
  , status               :: Boolean
  , cookie               :: Boolean
  , frictionlessRequests :: Boolean
  , hideFlashCallback    :: Boolean
  , autoLogAppEvents     :: Boolean
  , xmlfb                :: Boolean
  , debug                :: Boolean
  , locale               :: String
  }

defaultConfig :: AppId -> Config
defaultConfig appId = Config
  { appId                : appId
  , version              : "v2.10"
  , status               : false
  , cookie               : false
  , frictionlessRequests : false
  , hideFlashCallback    : false
  , autoLogAppEvents     : false
  , xmlfb                : false
  , debug                : false
  , locale               : "en_US"
  }

derive instance eqConfig :: Eq Config
derive instance genericConfig :: Generic Config
instance showConfig :: Show Config where show = gShow

data Status = Connected | NotAuthorized | Unknown

derive instance eqStatus :: Eq Status
derive instance genericStatus :: Generic Status
instance showStatus :: Show Status where show = gShow

newtype StatusInfo = StatusInfo
  { status       :: Status
  , authResponse :: Maybe AuthResponse
  }

derive instance eqStatusInfo :: Eq StatusInfo
derive instance genericStatusInfo :: Generic StatusInfo
instance showStatusInfo :: Show StatusInfo where show = gShow

newtype AccessToken = AccessToken String
derive newtype instance eqAccessToken :: Eq AccessToken
derive newtype instance showAccessToken :: Show AccessToken
derive newtype instance encodeAccessToken :: Encode AccessToken
derive instance genericAccessToken :: Generic AccessToken

newtype UserId = UserId String
derive newtype instance eqUserId :: Eq UserId
derive newtype instance showUserId :: Show UserId
derive newtype instance encodeUserId :: Encode UserId
derive instance genericUserId :: Generic UserId

newtype UserName = UserName String
derive newtype instance eqUserName :: Eq UserName
derive newtype instance showUserName :: Show UserName
derive instance genericUserName :: Generic UserName

newtype UserEmail = UserEmail String
derive newtype instance eqUserEmail :: Eq UserEmail
derive newtype instance showUserEmail :: Show UserEmail
derive instance genericUserEmail :: Generic UserEmail

newtype AuthResponse = AuthResponse
  { accessToken   :: AccessToken
  , expiresIn     :: Int
  , signedRequest :: String
  , userId        :: UserId
  }

derive instance eqAuthResponse :: Eq AuthResponse
derive instance genericAuthResponse :: Generic AuthResponse
instance showAuthResponse :: Show AuthResponse where show = gShow

data Field = Id | Name | Email | Gender | Birthday
derive instance eqField :: Eq Field
derive instance genericField :: Generic Field
derive instance ordField :: Ord Field
instance showField :: Show Field where
  show Id = "Id"
  show Name = "Name"
  show Email = "Email"
  show Gender = "Gender"
  show Birthday = "Birthday"

readField :: String -> Either String Field
readField "id" = Right Id
readField "name" = Right Name
readField "email" = Right Email
readField "gender" = Right Gender
readField "birthday" = Right Birthday
readField field = Left $ "Unknown field in the Facebook Graph API response (" <> field <> ")"

newtype UserInfo = UserInfo
  { id    :: UserId
  , name  :: UserName
  , email :: UserEmail
  }

derive instance eqUserInfo :: Eq UserInfo

data Sdk

newtype Scope = Scope String
derive newtype instance eqScope :: Eq Scope
derive instance newtypeScope :: Newtype Scope _

newtype LoginOptions = LoginOptions
  { scopes :: Array Scope
  }
instance encodeLoginOptions :: Encode LoginOptions where
  encode (LoginOptions { scopes: sx }) =
    encode $ SM.fromFoldable [Tuple "scope" (encode $ joinWith "," (map unwrap sx))]

instance showSdk :: Show Sdk where
  show = const "[Facebook SDK]"


readStatusInfo :: Foreign -> F StatusInfo
readStatusInfo value = do
  st <- value ! "status" >>= readStatus
  ar <- value ! "authResponse" >>= readNullOrUndefined >>= traverse readAuthResponse
  pure $ StatusInfo { status: st, authResponse: ar}
  where
    readStatus :: Foreign -> F Status
    readStatus status = do
      str <- readString status
      case str of
        "connected"      -> pure Connected
        "not_authorized" -> pure NotAuthorized
        otherwise        -> pure Unknown
    readAuthResponse :: Foreign -> F AuthResponse
    readAuthResponse authResponse = do
      at <- authResponse ! "accessToken" >>= readString <#> AccessToken
      ei <- authResponse ! "expiresIn" >>= readInt
      sr <- authResponse ! "signedRequest" >>= readString
      id <- authResponse ! "userID" >>= readString <#> UserId
      pure $ AuthResponse { accessToken: at
                          , expiresIn: ei
                          , signedRequest: sr
                          , userId: id
                          }

type SMap = SM.StrMap String

readSMap :: Foreign -> F SMap
readSMap value = do
  ks <- keys value
  vs <- traverse (\key -> value ! key >>= readString) ks
  pure $ SM.fromFoldable (zip ks vs)

-- | Initialize Facebook SDK
-- init :: forall e. Config -> Aff e Sdk
-- init config = makeAff (\error success -> _init success config)
init :: forall eff. Config -> Aff eff Sdk
init = _init >>> fromEffFnAff

-- | Retrieve a Facebook Login status
loginStatus :: forall eff. Sdk -> Aff eff StatusInfo
loginStatus sdk = _loginStatus sdk # fromEffFnAff >>= returnStatusInfo

-- | Login user
login :: forall eff. Sdk -> LoginOptions -> Aff eff StatusInfo
login sdk opts = encode opts # _login sdk # fromEffFnAff >>= returnStatusInfo

-- | Logout user
logout :: forall e. Sdk -> Aff e StatusInfo
logout sdk = _logout sdk # fromEffFnAff >>= returnStatusInfo

type ApiPath = String
type ApiParams = SMap
type ApiPathF = Foreign
type ApiParamsF = Foreign
type ApiMethodF = Foreign

data ApiMethod = Get | Post | Delete
derive instance eqApiMethod :: Eq ApiMethod
derive instance genericApiMethod :: Generic ApiMethod
instance showApiMethod :: Show ApiMethod where show = gShow

instance encodeApiMethod :: Encode ApiMethod where
  encode Get = encode "get"
  encode Post = encode "post"
  encode Delete = encode "delete"


returnStatusInfo :: forall e. Foreign -> Aff e StatusInfo
returnStatusInfo value = do
  either (handleForeignErrors "Facebook status info") pure $
    runExcept (readStatusInfo value)

-- | String is an error message prefix
handleForeignErrors :: forall e a. String -> MultipleErrors -> Aff e a
handleForeignErrors prefix errors =
  let message = prefix <> ": " <> intercalate "; " (map show errors)
  in throwError $ error message

-- | https://developers.facebook.com/docs/javascript/reference/.init/v2.10
-- foreign import _init :: forall e. (Sdk -> Eff e Unit) -> Config -> Eff e Unit
foreign import _init :: forall eff. Config -> EffFnAff eff Sdk

-- | https://developers.facebook.com/docs/reference/javascript/FB.login
-- foreign import _login :: forall e. Foreign -> Sdk -> (Foreign -> Eff e Unit) -> Eff e Unit
foreign import _login :: forall eff. Sdk -> Foreign -> EffFnAff eff Foreign

-- | https://developers.facebook.com/docs/reference/javascript/FB.logout
foreign import _logout :: forall eff. Sdk -> EffFnAff eff Foreign

-- | https://developers.facebook.com/docs/reference/javascript/FB.getLoginStatus
foreign import _loginStatus :: forall eff. Sdk -> EffFnAff eff Foreign
