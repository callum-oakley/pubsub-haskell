{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Data.Maybe             as Maybe
import qualified Network.Socket         as Socket
import qualified System.Environment     as Environment
import qualified Text.Read              as Read

import qualified Bus
import           Server                 (Server (..))
import qualified Server

main :: IO ()
main = do
  server <- Server <$> STM.atomically Bus.newBus
  env <- Environment.lookupEnv "PORT"
  let port = maybe 8081 readPort env :: Int
  addr <-
    head <$>
    Socket.getAddrInfo
      (Just Socket.defaultHints {Socket.addrSocketType = Socket.Stream})
      (Just "localhost")
      (Just $ show port)
  socket <-
    Socket.socket
      (Socket.addrFamily addr)
      (Socket.addrSocketType addr)
      (Socket.addrProtocol addr)
  Socket.setSocketOption socket Socket.ReuseAddr 1
  Socket.bind socket $ Socket.addrAddress addr
  Socket.listen socket 1024
  Server.listen server socket
  where
    readPort =
      Maybe.fromMaybe (error "failed to parse PORT as Int") . Read.readMaybe
