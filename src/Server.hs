{-# LANGUAGE OverloadedStrings #-}

module Server
  ( Server(..)
  , listen
  ) where

import           Control.Concurrent.Async  (async)
import qualified Control.Concurrent.STM    as STM
import           Control.Monad             (forever)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Encoding
import           Network.Socket            (Socket)
import qualified Network.Socket            as Socket
import qualified Network.Socket.ByteString as SBS

import           Bus                       (Bus)
import qualified Bus
import           Request                   (Request (..))
import qualified Request

data Server =
  Server Bus

bus :: Server -> Bus
bus (Server b) = b

listen :: Server -> Socket -> IO ()
listen server socket = do
  (conn, _) <- Socket.accept socket
  _ <- async $ handle server conn
  listen server socket

handle :: Server -> Socket -> IO ()
handle server conn = do
  request <- Request.newRequest conn
  case request of
    Left err -> do
      SBS.sendAll conn $
        Encoding.encodeUtf8 $
        Text.concat ["ERR\r\nfailed to parse request: ", err, "\r\n"]
      handle server conn
    Right (Pub channel message) -> handlePub server conn channel message
    Right (Sub channel) -> handleSub server conn channel

handlePub :: Server -> Socket -> Bus.Channel -> Bus.Message -> IO ()
handlePub server conn channel message = do
  SBS.sendAll conn "ACK\r\n"
  STM.atomically $ Bus.publish (bus server) channel message
  handle server conn

-- TODO handle disconnects
handleSub :: Server -> Socket -> Bus.Channel -> IO ()
handleSub server conn channel = do
  SBS.sendAll conn "ACK\r\n"
  queue <- STM.atomically $ Bus.subscribe (bus server) channel
  forever $ do
    message <- STM.atomically $ STM.readTQueue queue
    SBS.sendAll conn $
      Encoding.encodeUtf8 $ Text.concat ["MSG\r\n", message, "\r\n"]
