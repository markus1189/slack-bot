{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Lens (view,strict)
import           Control.Lens.Operators hiding ((.=))
import           Control.Monad.State (runStateT)
import           Control.Monad.State.Class (MonadState, put, get)
import           Data.Aeson (Value(..), object, (.=), eitherDecode)
import           Data.Aeson.Encode (encode)
import           Data.Aeson.Lens
import           Data.ByteString.Lazy (ByteString)
import           Data.Coerce (coerce)
import           Data.Either (isRight)
import           Data.Either.Combinators (fromRight')
import           Data.Foldable (traverse_)
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.WebSockets (sendClose, receiveData, sendTextData, Connection, WebSocketsData)
import           Network.Wreq (responseBody)
import qualified Network.Wreq as Wreq
import           Pipes
import qualified Pipes.Concurrent as P
import qualified Pipes.Prelude as P
import           System.Environment (getArgs)
import           Wuss

import           Network.Slack.Types

data State = State {stateReconnectUrl :: Maybe ReconnectUrl
                   ,stateUserPresence :: Map Text Presence
                   ,stateIdToName :: Map Text Text
                   ,stateNextId :: Integer
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
  resp <- Wreq.get ("https://slack.com/api/rtm.start?token=" <> token)
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
              1
  runSecureClient host 443 path $ \c -> do
    (output,input) <- wsProdCons (P.newest 50) c
    flip runStateT state $ runEffect $ P.fromInput input
            >-> P.tee (P.map (T.unpack . T.decodeUtf8 . view strict) >-> P.stdoutLn)
            >-> P.map parse
            >-> P.mapM_ (traverse_ (eventReply output state))
    sendClose c ("Goodbye" :: Text)

  where parse :: ByteString -> Either String Event
        parse = eitherDecode

prepareMessage :: MonadState State m => Text -> Text -> m Value
prepareMessage chan txt = do
  s <- get
  put (s { stateNextId = stateNextId s + 1})
  let i = stateNextId s
  return $ object ["id" .= T.pack (show i)
                  ,"type" .= ("message" :: Text)
                  ,"channel" .= chan
                  ,"text" .= show txt]

eventReply :: (MonadIO m, MonadState State m) => P.Output ByteString -> State -> Event -> m ()
eventReply output s (PresenceChangeEvt usr p) = do
  msg <- prepareMessage (translateId s "bot-test") $ translateId s (coerce usr) <> " -> " <> T.pack (show p)
  void (liftIO (P.atomically (P.send output (encode msg))))
eventReply output s (MessageEvt mchan usr txt _) = do
  let recipient = fromMaybe (coerce usr) mchan
  msg <- prepareMessage recipient $ "Hi " <> translateId s (coerce usr) <> " I can hear you."
  void (liftIO (P.atomically (P.send output (encode msg))))
eventReply _ _ _ = return ()

translateId :: State -> Text -> Text
translateId state i = Map.findWithDefault i i (stateIdToName state)
