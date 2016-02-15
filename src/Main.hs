{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Lens (view,strict)
import           Control.Lens.Operators hiding ((.=))
import           Control.Monad (forever, mzero)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), object, (.=), Object, eitherDecode)
import           Data.Aeson.Encode (encode)
import           Data.Aeson.Lens
import           Data.ByteString.Lazy (ByteString)
import           Data.Either (isRight)
import           Data.Either.Combinators (fromRight')
import           Data.Function (on)
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>), Last(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Network.WebSockets (ClientApp, sendClose, receiveData, sendTextData, Connection, WebSocketsData)
import           Network.Wreq (get,responseBody)
import           Pipes
import qualified Pipes.Concurrent as P
import qualified Pipes.Prelude as P
import qualified Pipes.WebSockets as P
import           System.Environment (getArgs)
import           Wuss

import Network.Slack.Types

t :: Text -> Text
t = id

data State = State { stateReconnectUrl :: Maybe ReconnectUrl
                   , stateUserPresence :: Map Text Presence }

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
  let uri = resp ^. responseBody . key "url" . _String
  let (_:_:host:paths) = splitOn "/" (T.unpack uri)
      path = concatMap ('/':) paths
  runSecureClient host 443 path $ \c -> do
    (output,input) <- wsProdCons (P.newest 50) c
    runEffect $ P.fromInput input >-> P.tee (P.map (T.unpack . T.decodeUtf8 . view strict) >-> P.stdoutLn) >-> P.map parse >-> P.filter isRight >-> P.map fromRight' >-> P.filter (\e -> case e of
                                                                                                                                                                                                    PresenceChangeEvt _ _ -> True
                                                                                                                                                                                                    _ -> False) >-> P.map (\(PresenceChangeEvt usr p) -> prepareMessage "C0MCQ870A" ("Presence change: " <> usr <> " to " <> T.pack (show p))) >-> P.map encode >-> P.toOutput output
    undefined

  where parse :: ByteString -> Either String Event
        parse = eitherDecode

prepareMessage :: Text -> Text -> Value
prepareMessage chan txt = object ["id" .= ("42" :: Text)
                                 , "type" .= ("message" :: Text)
                                 ,"channel" .= chan
                                 ,"text" .= show txt]
