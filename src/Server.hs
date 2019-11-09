{-# LANGUAGE OverloadedStrings #-}

module Server
  ( Server(..)
  , listen
  ) where

import           Control.Concurrent.Async (async)
import qualified Control.Concurrent.STM   as STM
import           Control.Monad            (forever)
import qualified Data.List                as List
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as TIO
import           Network.Socket           (Socket)
import qualified Network.Socket           as Socket
import           System.IO                (Handle)
import qualified System.IO                as IO

import           Bus                      (Bus)
import qualified Bus
import           Request                  (Request (..))
import qualified Request

newtype Server =
  Server Bus

bus :: Server -> Bus
bus (Server b) = b

listen :: Server -> Socket -> IO ()
listen server socket = do
  (socket', _) <- Socket.accept socket
  conn <- Socket.socketToHandle socket' IO.ReadWriteMode
  _ <- async $ handle server conn
  listen server socket

handle :: Server -> Handle -> IO ()
handle server conn = do
  request <- Request.newRequest conn
  case request of
    Left err -> do
      logTexts ["ERR", err]
      TIO.hPutStr conn $
        Text.concat ["ERR\r\nfailed to parse request: ", err, "\r\n"]
      handle server conn
    Right (Pub channel message) -> handlePub server conn channel message
    Right (Sub channel) -> handleSub server conn channel

handlePub :: Server -> Handle -> Bus.Channel -> Bus.Message -> IO ()
handlePub server conn channel message = do
  logTexts ["PUB", channel, message]
  TIO.hPutStr conn "ACK\r\n"
  STM.atomically $ Bus.publish (bus server) channel message
  handle server conn

-- TODO handle disconnects
handleSub :: Server -> Handle -> Bus.Channel -> IO ()
handleSub server conn channel = do
  logTexts ["SUB", channel]
  TIO.hPutStr conn "ACK\r\n"
  queue <- STM.atomically $ Bus.subscribe (bus server) channel
  forever $ do
    message <- STM.atomically $ STM.readTQueue queue
    TIO.hPutStr conn $ Text.concat ["MSG\r\n", message, "\r\n"]

logTexts :: [Text] -> IO ()
logTexts ts =
  TIO.putStr $ Text.append (Text.concat $ List.intersperse " " ts) "\n"
