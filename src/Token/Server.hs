{-# LANGUAGE QuasiQuotes       #-}
module Token.Server where

import Auth.Biscuit.Servant
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Data.Text (Text)
import Data.Text.Encoding
import Data.Maybe
import Data.UUID (UUID)
import Colourista.IO
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Network.Wai.Handler.Warp as Warp
import Data.Time (UTCTime)
import qualified Data.Time as Time

-------------------
-- API Datatypes --
-------------------

type TokenM = ReaderT () Handler

type API = NamedRoutes TokenAPI

data TokenAPI mode = TokenAPI
  { getToken :: mode :- "tokens" :> ReqBody '[JSON] GetTokenRequest :> Post '[JSON] GetTokenResponse
  }
  deriving stock Generic

data GetTokenRequest = GetTokenRequest
  { email :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

data GetTokenResponse = GetTokenResponse
  { token :: Text
  , timestamp :: UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

startServer :: IO ()
startServer = do
  blueMessage "[+] Starting the Token dispenser on http://localhost:8900"
  let warpSettings = Warp.setPort 8900 Warp.defaultSettings
  Warp.runSettings warpSettings server

server :: Application
server =
  genericServeT (`runReaderT` ()) tokenServer

tokenServer :: TokenAPI (AsServerT TokenM)
tokenServer = TokenAPI
  { getToken = handleGetToken
  }

handleGetToken :: GetTokenRequest -> TokenM GetTokenResponse
handleGetToken GetTokenRequest{email} = 
  createToken email
  >>= \case
    Nothing -> throwError err403
    Just biscuit -> do
      currentTimestamp <- liftIO Time.getCurrentTime
      let timestamp = Time.addUTCTime 300 currentTimestamp
      finalBiscuit <- liftIO $ addBlock [block| time(${timestamp}); |] biscuit
      let token = decodeUtf8 $ serializeB64 finalBiscuit
      let tokenResponse = GetTokenResponse token timestamp
      pure tokenResponse

createToken :: Text -> TokenM (Maybe (Biscuit Open Verified))
createToken "hr@example.org" = do
  biscuit <- liftIO $ mkBiscuit privateKey'
              [block| 
                check if
                  service("peopledoc");
                  right("read", "employees");
              |]
  pure $ Just biscuit

createToken "admin@example.org" = do
  biscuit <- liftIO $ mkBiscuit privateKey'
              [block|
                user_id("ab53e7eb-7ff2-438d-b4db-0cd1013d9a68");
                right("read", "user_group", "5dd98b37-01df-44ad-8a3b-2d86b58053b1");
                right("write", "user_group", "5dd98b37-01df-44ad-8a3b-2d86b58053b1");
                service("api");
              |]
  pure $ Just biscuit

createToken "employee@example.org" = do
  biscuit <- liftIO $ mkBiscuit privateKey'
          [block| 
            user_id("2d38f260-09a6-46ef-9d27-a4c6e2b21381");
            check if
              service("api");
              right("read", "user_in_usergroup", "2d38f260-09a6-46ef-9d27-a4c6e2b21381", "5dd98b37-01df-44ad-8a3b-2d86b58053b1");
              right("write", "user_in_usergroup", "2d38f260-09a6-46ef-9d27-a4c6e2b21381", "5dd98b37-01df-44ad-8a3b-2d86b58053b1");
          |]
  pure $ Just biscuit
createToken _ =
  pure Nothing

data Permission = Permission Action Resource
  deriving stock (Show, Eq, Ord, Generic)

data Action = Read | Write
  deriving stock (Show, Eq, Ord, Generic)

newtype UserGroupId = UserGroupId UUID
  deriving stock (Show, Eq, Ord, Generic)

newtype UserId = UserId UUID
  deriving stock (Show, Eq, Ord, Generic)

data Resource = UserGroup UserGroupId
              | UserInUserGroup UserId UserGroupId
              | EmployeeFile UUID
  deriving stock (Show, Eq, Ord, Generic)

-- | We can translate those into blocks but it is instead done manually in the quasi-quoter
permissions :: Map UserId [Permission]
permissions = Map.fromList
  [ (user1, [ Permission Read (UserInUserGroup user1 user1UserGroup)
            , Permission Write (UserInUserGroup user1 user1UserGroup)
            ])
  , (admin1, [ Permission Write (UserGroup user1UserGroup)
             ])
  ]

user1 :: UserId
user1 = UserId $ read "2d38f260-09a6-46ef-9d27-a4c6e2b21381"

user1UserGroup :: UserGroupId
user1UserGroup = UserGroupId $ read "5dd98b37-01df-44ad-8a3b-2d86b58053b1"

admin1 :: UserId
admin1 = UserId $ read "ab53e7eb-7ff2-438d-b4db-0cd1013d9a68"

employee1 :: UUID
employee1 = read "1e1d62d8-a508-4d75-846b-fd5c646d6daf"


privateKey' :: SecretKey
privateKey' = fromMaybe (error "Error parsing private key") $ parseSecretKeyHex "48e9c8b68441134b7f6e10b67df968f5a0837560d625948b2cbd5d80ffed092e"

publicKey' :: PublicKey
publicKey' = fromMaybe (error "Error parsing public key") $ parsePublicKeyHex "18a09fde27cdd6687acce4c6e2ef930f1073911ba7f8d213e447bc211d2be96e"
