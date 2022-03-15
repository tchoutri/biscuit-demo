{-# LANGUAGE QuasiQuotes       #-}
module API.Server where

import Auth.Biscuit.Servant
import Servant
import Servant.Server.Internal
import Servant.API.Generic
import Data.Maybe
import Colourista.IO
import Control.Monad.Reader (ReaderT, runReaderT, liftIO)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Time as Time
import Network.Wai.Middleware.Cors
-- import Network.Wai.Middleware.RequestLogger 

import Types

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

startServer :: IO ()
startServer = do
  blueMessage "[+] Starting the API server on http://localhost:8902"
  Warp.run 8902 $
    cors (const $ Just policy) apiApp
      where
        policy = simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type", "Authorization"]
          , corsOrigins = Nothing
          }

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
        time(${timestamp});
      |]
    $ app
    

apiHandlers :: ProtectedAPI (AsServerT APIM)
apiHandlers = ProtectedAPI
  { showUser = showUserHandler
  , showGroup = showGroupHandler
  }

showUserHandler :: UserGroupId -> UserId -> APIM UserInfo
showUserHandler userGroupId userId =
  withAuthorizer [authorizer| allow if right("read", "user_in_usergroup", ${userId}, ${userGroupId}); |] $ do
    pure $ UserInfo{userId = userId, name = "Bertrand PLASTIC"}

showGroupHandler :: UserGroupId -> APIM UserGroupInfo
showGroupHandler userGroupId =
  withAuthorizer [authorizer| allow if right("read", "user_group", ${userGroupId}); |] $ do
    pure UserGroupInfo{userGroupId = userGroupId, name = "Passing the time"}


publicKey' :: PublicKey
publicKey' = fromMaybe (error "Error parsing public key") $ parsePublicKeyHex "18a09fde27cdd6687acce4c6e2ef930f1073911ba7f8d213e447bc211d2be96e"
