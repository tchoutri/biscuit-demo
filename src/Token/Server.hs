{-# LANGUAGE QuasiQuotes       #-}
module Token.Server where

import Auth.Biscuit.Servant
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Data.Text (Text)
import Data.Text.Encoding
import Data.Maybe
import Colourista.IO
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Time as Time

import Types

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
      let token = decodeUtf8 $ serializeB64 biscuit
      let tokenResponse = GetTokenResponse token
      pure tokenResponse

createToken :: Text -> TokenM (Maybe (Biscuit Open Verified))
createToken email =
  case Map.lookup email permissions of
    Nothing -> pure Nothing
    Just perms -> do
      case Map.lookup email users of
        Nothing -> pure Nothing
        Just userId -> do
          currentTimestamp <- liftIO Time.getCurrentTime
          let timestamp = Time.addUTCTime 300 currentTimestamp
          let authority = [block|
            service("api");
            user_id(${userId});
            check if time($t), $t < ${timestamp};
            |]<> foldMap permissionToBlock perms
          biscuit <- liftIO $ mkBiscuit privateKey' authority
          pure $ Just biscuit

data Permission = Permission Action Resource
  deriving stock (Show, Eq, Ord, Generic)

data Action = Read | Write
  deriving stock (Show, Eq, Ord, Generic)

data Resource = UserGroup UserGroupId
              | UserInUserGroup UserId UserGroupId
              | EmployeeFile UserId
  deriving stock (Show, Eq, Ord, Generic)

-- | We can translate those into blocks but it is instead done manually in the quasi-quoter
permissions :: Map Text [Permission]
permissions = Map.fromList
  [ ("employee@example.org", [ Permission Read (UserInUserGroup employee1 employee1UserGroup)
                             , Permission Write (UserInUserGroup employee1 employee1UserGroup)
                             ])
  , ("admin@example.org", [ Permission Write (UserGroup employee1UserGroup)
                          , Permission Read (UserGroup employee1UserGroup)
                          , Permission Read (UserInUserGroup employee1 employee1UserGroup)
                          , Permission Write (UserInUserGroup employee1 employee1UserGroup)
                          ])
  ]

users :: Map Text UserId
users = Map.fromList
  [ ("employee@example.org", UserId $ read "2d38f260-09a6-46ef-9d27-a4c6e2b21381")
  , ("admin@example.org", UserId $ read "ab53e7eb-7ff2-438d-b4db-0cd1013d9a68")
  ]

permissionToBlock :: Permission -> Block
permissionToBlock (Permission Read (UserInUserGroup userId userGroupId)) = [block| right("read", "user_in_usergroup", ${userId}, ${userGroupId}); |]
permissionToBlock (Permission Write (UserInUserGroup userId userGroupId)) = [block| right("write", "user_in_usergroup", ${userId}, ${userGroupId}); |]
permissionToBlock (Permission Read (UserGroup userGroupId)) = [block| right("read", "user_group", ${userGroupId}); |]
permissionToBlock (Permission Write (UserGroup userGroupId)) = [block| right("write", "user_group", ${userGroupId}); |]
permissionToBlock (Permission Write (EmployeeFile employeeFileId)) = [block| right("write", "employee_file", ${employeeFileId}); |]
permissionToBlock (Permission Read (EmployeeFile employeeFileId)) = [block| right("read", "employee_file", ${employeeFileId}); |]

employee1UserGroup :: UserGroupId
employee1UserGroup = UserGroupId $ read "5dd98b37-01df-44ad-8a3b-2d86b58053b1"

admin1 :: UserId
admin1 = UserId $ read "ab53e7eb-7ff2-438d-b4db-0cd1013d9a68"

employee1 :: UserId
employee1 = UserId $ read "1e1d62d8-a508-4d75-846b-fd5c646d6daf"

privateKey' :: SecretKey
privateKey' = fromMaybe (error "Error parsing private key") $ parseSecretKeyHex "48e9c8b68441134b7f6e10b67df968f5a0837560d625948b2cbd5d80ffed092e"

publicKey' :: PublicKey
publicKey' = fromMaybe (error "Error parsing public key") $ parsePublicKeyHex "18a09fde27cdd6687acce4c6e2ef930f1073911ba7f8d213e447bc211d2be96e"
