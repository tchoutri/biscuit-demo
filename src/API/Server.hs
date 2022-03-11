{-# LANGUAGE QuasiQuotes       #-}
module API.Server where

import Auth.Biscuit.Servant
import Servant
import Servant.Server.Internal
import Servant.API.Generic
import Data.Text (Text)
import Data.Maybe
import Data.UUID (UUID)
import Colourista.IO
import Control.Monad.Reader (ReaderT, runReaderT, liftIO)
import Data.Aeson
import qualified Network.Wai.Handler.Warp as Warp
import Data.Text.Display (Display(..), display, ShowInstance(..))
import qualified Data.Time as Time

-------------------
-- API Datatypes --
-------------------

type APIM = WithAuthorizer (ReaderT () Handler)

type API = RequireBiscuit :> NamedRoutes ProtectedAPI

data ProtectedAPI mode = ProtectedAPI
  { showGroup :: mode :- "user_groups"
                :> Capture "user_group_id" UserGroupId
                :> Get '[JSON] UserGroupInfo
  , showUser :: mode :- "user_groups"
             :> Capture "user_group_id" UserGroupId
             :> "users"
             :> Capture "user_id" UserId
             :> Get '[JSON] UserInfo
  }
  deriving stock Generic

newtype UserGroupId = UserGroupId UUID
  deriving stock (Eq, Ord, Generic)
  deriving (Show, FromHttpApiData, ToJSON)
    via UUID
  deriving Display
    via ShowInstance UserGroupId

instance ToTerm UserGroupId where
  toTerm = LString . display


newtype UserId = UserId UUID
  deriving stock (Eq, Ord, Generic)
  deriving (Show, FromHttpApiData, ToJSON)
    via UUID
  deriving Display
    via ShowInstance UserId

instance ToTerm UserId where
  toTerm = LString . display

data UserInfo = UserInfo
  { userId :: UserId
  , name :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

data UserGroupInfo = UserGroupInfo
  { userGroupId :: UserGroupId
  , name :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

startServer :: IO ()
startServer = do
  blueMessage "[+] Starting the API server on http://localhost:8902"
  Warp.run 8902 apiApp

apiApp :: Application
apiApp = serveWithContext @API Proxy (genBiscuitCtx publicKey') server

server :: Biscuit OpenOrSealed Verified -> ProtectedAPI (AsServerT Handler)
server b = hoistServer @(NamedRoutes ProtectedAPI) Proxy (naturalTransform b) apiHandlers

naturalTransform :: Biscuit OpenOrSealed Verified -> APIM a -> Handler a
naturalTransform b app = do
  timestamp <- liftIO Time.getCurrentTime
  (\r -> runReaderT r ())
    . handleBiscuit b
    . withPriorityAuthorizer [authorizer|
        check if
          service("api");
        check if
          time($time),
          $time <= ${timestamp};
      |]
    $ app

apiHandlers :: ProtectedAPI (AsServerT APIM)
apiHandlers = ProtectedAPI
  { showUser = showUserHandler
  , showGroup = showGroupHandler
  }

showUserHandler :: UserGroupId -> UserId -> APIM UserInfo
showUserHandler userGroupId userId =
  withAuthorizer [authorizer| allow if right("read", "user_in_usergroup", ${userGroupId}, ${userId}); |] $ do
    pure $ UserInfo{userId = userId, name = "Bertrand PLASTIC"}

showGroupHandler :: UserGroupId -> APIM UserGroupInfo
showGroupHandler userGroupId =
  withAuthorizer [authorizer| allow if right("read", "user_group", ${userGroupId}); |] $ do
    pure UserGroupInfo{userGroupId = userGroupId, name = "Passing the time"}


publicKey' :: PublicKey
publicKey' = fromMaybe (error "Error parsing public key") $ parsePublicKeyHex "18a09fde27cdd6687acce4c6e2ef930f1073911ba7f8d213e447bc211d2be96e"
