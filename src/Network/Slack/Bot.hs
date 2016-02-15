module Network.Slack.Bot (State (..)
                         ) where

import Data.Text (Text)
import Data.Map (Map)

import Network.Slack.Types

data State = State { stateReconnectUrl :: Maybe ReconnectUrl
                   , stateUserPresence :: Map Text Presence }
