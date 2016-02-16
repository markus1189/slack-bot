{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Slack.Types (ReconnectUrl (reconnectUrl)
                           ,Presence(..)
                           ,Channel(..)
                           ,ValueWithOrigin(..)
                           ,Event(..)
                           ,User(..)
                           ,UserId(..)
                           ,TeamId(..)
                           ) where

import Control.Monad (mzero)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), object, (.=), Object)
import Data.Aeson.TH
import Data.String (IsString)
import Data.Text (Text)
import Network.Slack.TH (jsonOptionsWithPrefix,jsonOptionsEvent)

newtype ReconnectUrl = ReconnectUrl { reconnectUrl :: Text }
                       deriving (Show,Eq,ToJSON,FromJSON,IsString)

newtype UserId = UserId Text deriving (Show,Eq,Ord,FromJSON,ToJSON,IsString)
newtype TeamId = TeamId Text deriving (Show,Eq,Ord,FromJSON,ToJSON,IsString)

data Presence = Active | Away deriving (Show,Eq)
instance FromJSON Presence where
  parseJSON (String "active") = pure Active
  parseJSON (String "away") = pure Away
  parseJSON _ = mzero

instance ToJSON Presence where
  toJSON Active = String "active"
  toJSON Away = String "away"

data Event = PresenceChangeEvt { presUser :: UserId, presPresence :: Presence }
           | ReconnectUrlEvt { reconUrl :: ReconnectUrl }
           | MessageEvt {msgChannel :: Maybe Text
                        ,msgUser :: UserId
                        ,msgText :: Text
                        ,msgTs :: Text
                        }
           | UnknownEvt Object
           deriving (Show,Eq)
$(deriveJSON jsonOptionsEvent ''Event)

data ValueWithOrigin = ValueWithOrigin {vwoValue :: Text
                                       ,vwoCreator :: UserId
                                       ,vwoLastSet :: Integer
                                       } deriving (Show,Eq)
instance FromJSON ValueWithOrigin where
  parseJSON (Object o) = ValueWithOrigin <$> o .: "value"
                                         <*> o .: "creator"
                                         <*> o .: "last_set"
  parseJSON _ = mzero
instance ToJSON ValueWithOrigin where
  toJSON (ValueWithOrigin v c l) = object ["value" .= v
                                          ,"creator" .= c
                                          ,"last_set" .= l
                                          ]

data Channel = Channel {chanId :: Text
                       ,chanName :: Text
                       ,chanIsChannel :: Bool
                       ,chanCreated :: Integer
                       ,chanCreator :: UserId
                       ,chanIsArchived :: Bool
                       ,chanIsGeneral :: Bool
                       ,chanHasPins :: Bool
                       ,chanIsMember :: Bool
                       ,chanLastRead :: Maybe Text
                       ,chanLatest :: Maybe Event
                       ,chanUnreadCount :: Maybe Integer
                       ,chanUnreadCountDisplay :: Maybe Integer
                       ,chanMembers :: Maybe [UserId]
                       ,chanTopic :: Maybe ValueWithOrigin
                       ,chanPurpose :: Maybe ValueWithOrigin
                       } deriving (Show,Eq)

$(deriveJSON (jsonOptionsWithPrefix "chan") ''Channel)

data User = User {userId :: UserId
                 ,userTeamId :: TeamId
                 ,userName :: Text
                 ,userDeleted :: Bool
                 ,userColor :: Text
                 ,userRealName :: Text
                 ,userIsAdmin :: Bool
                 ,userIsOwner :: Bool
                 ,userIsPrimaryOwner :: Bool
                 ,userIsRestricted :: Bool
                 ,userIsUltraRestricted :: Bool
                 ,userIsBot :: Bool
                 ,userPresence :: Presence
                 } deriving (Show,Eq)
$(deriveJSON (jsonOptionsWithPrefix "user") ''User)
