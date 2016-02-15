{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Lens (view,strict)
import           Control.Lens.Operators hiding ((.=))
import           Data.Aeson (Value(..), object, (.=), eitherDecode)
import           Data.Aeson.Encode (encode)
import           Data.Aeson.Lens
import           Data.ByteString.Lazy (ByteString)
import           Data.Coerce (coerce)
import           Data.Either (isRight)
import           Data.Either.Combinators (fromRight')
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.WebSockets (sendClose, receiveData, sendTextData, Connection, WebSocketsData)
import           Network.Wreq (get,responseBody)
import           Pipes
import qualified Pipes.Concurrent as P
import qualified Pipes.Prelude as P
import           System.Environment (getArgs)
import           Wuss

import           Network.Slack.Types

data State = State {stateReconnectUrl :: Maybe ReconnectUrl
                   ,stateUserPresence :: Map Text Presence
                   ,stateIdToName :: Map Text Text
                   }

wsProd :: (WebSocketsData a, MonadIO m) => Connection -> Producer a m ()
wsProd c = P.repeatM (liftIO (receiveData c))

wsCons :: (WebSocketsData a, MonadIO m) => Connection -> Consumer a m ()
wsCons c = for cat (liftIO . sendTextData c)

wsProdCons :: WebSocketsData a
           => P.Buffer a
           -> Connection
           -> IO (P.Output a, P.Input a)
wsProdCons buf c = do
  (o1,i1) <- P.spawn buf
  (o2,i2) <- P.spawn buf
  void $ forkIO $ runEffect $ wsProd c >-> P.toOutput o1
  void $ forkIO $ runEffect $ P.fromInput i2 >-> wsCons c
  return (o2,i1)

main :: IO ()
main = do
  [token] <- getArgs
  resp <- get ("https://slack.com/api/rtm.start?token=" <> token)
  let body = resp ^. responseBody
      uri = body ^?! key "url" . _String
      users :: [User]
      users = body ^?! key "users" . _JSON
      channels :: [Channel]
      channels = body ^?! key "channels" . _JSON
      (_:_:host:paths) = splitOn "/" (T.unpack uri)
      path = concatMap ('/':) paths
      state =
        State Nothing
              Map.empty
              (Map.union
                 (Map.fromList (map (\u -> (coerce (userId u), userName u)) users))
                 (Map.fromList (map (\c -> (coerce (chanId c), chanName c)) channels)))
      translateId :: Text -> Text
      translateId i = Map.findWithDefault i i (stateIdToName state)
  runSecureClient host 443 path $ \c -> do
    (output,input) <- wsProdCons (P.newest 50) c
    runEffect $ P.fromInput input
            >-> P.tee (P.map (T.unpack . T.decodeUtf8 . view strict)
            >-> P.stdoutLn)
            >-> P.map parse
            >-> P.filter isRight
            >-> P.map fromRight'
            >-> P.filter (\e -> case e of
                                  PresenceChangeEvt _ _ -> True
                                  _ -> False)
            >-> P.map (\(PresenceChangeEvt usr p) ->
                         prepareMessage "C0MCQ870A" (translateId (coerce usr)
                                                  <> " -> "
                                                  <> T.pack (show p)))
            >-> P.map encode
            >-> P.toOutput output
    sendClose c ("Goodbye" :: Text)

  where parse :: ByteString -> Either String Event
        parse = eitherDecode

prepareMessage :: Text -> Text -> Value
prepareMessage chan txt = object ["id" .= ("42" :: Text)
                                 ,"type" .= ("message" :: Text)
                                 ,"channel" .= chan
                                 ,"text" .= show txt]
