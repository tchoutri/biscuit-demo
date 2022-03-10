{-# LANGUAGE QuasiQuotes       #-}
module API.Server where

import Auth.Biscuit.Servant
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Data.Text (Text)
import Data.Maybe
import Data.UUID (UUID)
import Colourista.IO
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson
import qualified Network.Wai.Handler.Warp as Warp
import Data.Text.Display (Display(..), display, ShowInstance(..))
import Servant.API.Generic
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Client.Core (Request)

-------------------
-- API Datatypes --
-------------------

type APIM = WithAuthorizer (ReaderT () Handler)

type API mode = RequireBiscuit :> NamedRoutes ProtectedAPI

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
  blueMessage "[+] Starting the Token issuer server on localhost:8900"
  Warp.run 9802 apiApp

apiApp :: Application
apiApp = serveWithContext p (genBiscuitCtx publicKey') apiServer
  where
    p = genericApi (Proxy :: Proxy APIM)

apiServer :: Biscuit OpenOrSealed Verified -> Server api
apiServer biscuit = genericServeT $ 
  hoistServerWithContext p pctx (naturalTransform biscuit) (genericServeT apiHandlers) 
    where
      p = genericApi (Proxy :: Proxy APIM)
      pctx = Proxy :: Proxy (Context '[AuthHandler Request (Biscuit OpenOrSealed Verified)])

naturalTransform :: Biscuit OpenOrSealed Verified -> APIM a -> Handler a
naturalTransform b app = 
  (\r -> runReaderT r ())
    $ handleBiscuit b
    $ withPriorityAuthorizer [authorizer|allow if service("api");|]
    app

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
