{-# LANGUAGE OverloadedStrings #-}

module Request
  ( Request(..)
  , newRequest
  ) where

import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Encoding
import           Network.Socket            (Socket)
import qualified Network.Socket.ByteString as SBS

import qualified Bus

data Request
  = Pub Bus.Channel Bus.Message
  | Sub Bus.Channel
  deriving (Show)

newRequest :: Socket -> IO (Either Text Request)
newRequest socket = do
  op <- readText socket
  case op of
    "PUB" -> do
      channel <- readText socket
      message <- readText socket
      return $ Right $ Pub channel message
    "SUB" -> do
      channel <- readText socket
      return $ Right $ Sub channel
    otherwise -> return $ Left $ Text.append "Unknown operation: " op

-- TODO handle messages larger than 4096 bytes
readText :: Socket -> IO Text
readText socket = Text.strip <$> Encoding.decodeUtf8 <$> SBS.recv socket 4096
