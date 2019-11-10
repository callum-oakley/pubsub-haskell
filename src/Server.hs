{-# LANGUAGE OverloadedStrings #-}

module Server
  ( Server(..)
  , listen
  ) where

import           Control.Concurrent.Async   (async)
import qualified Control.Concurrent.STM     as STM
import           Control.Monad              (forever)
import qualified Control.Monad.Trans        as MTrans
import           Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import qualified Data.Text                  as Text
import           Network.Socket             (Socket)
import qualified Network.Socket             as Socket
import           System.IO                  (Handle)
import qualified System.IO                  as IO

import           Bus                        (Bus)
import qualified Bus
import           Error                      (Error)
import           ExceptTErrorIO
import           Request                    (Request (..))
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
  res <-
    Except.runExceptT $ do
      request <- Request.newRequest conn
      case request of
        Pub channel message -> handlePub server conn channel message
        Sub channel         -> handleSub server conn channel
  case res of
    Left err -> do
      _ <-
        Except.runExceptT $ do
          putLine ["ERR", Text.pack $ show err]
          hPutLines conn ["ERR", Text.pack $ show err]
      IO.hClose conn
    Right _ -> return ()

handlePub ::
     Server -> Handle -> Bus.Channel -> Bus.Message -> ExceptT Error IO ()
handlePub server conn channel message = do
  putLine ["PUB", channel, message]
  hPutLines conn ["ACK"]
  MTrans.lift $ do
    STM.atomically $ Bus.publish (bus server) channel message
    Bus.debugReport (bus server)
    handle server conn

-- TODO better to use "bracket" to do the unsubscribe?
handleSub :: Server -> Handle -> Bus.Channel -> ExceptT Error IO ()
handleSub server conn channel = do
  putLine ["SUB", channel]
  hPutLines conn ["ACK"]
  (sub, unsubscribe) <-
    MTrans.lift $ STM.atomically $ Bus.subscribe (bus server) channel
  MTrans.lift $ Bus.debugReport (bus server)
  forever $
    Except.catchE
      (do message <- MTrans.lift $ STM.atomically $ STM.readTQueue sub
          hPutLines conn ["MSG", message])
      (\e -> do
         MTrans.lift $ STM.atomically unsubscribe
         Except.throwE e)
