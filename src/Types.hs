module Types where

import Data.UUID
import GHC.Generics
import Servant
import Data.Aeson
import Data.Text.Display
import Auth.Biscuit
import Data.Text


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
