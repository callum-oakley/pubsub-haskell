{-# LANGUAGE OverloadedStrings #-}

module Request
  ( Request(..)
  , newRequest
  ) where

import           Data.Text    (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as TIO
import           System.IO    (Handle)

import qualified Bus

data Request
  = Pub Bus.Channel Bus.Message
  | Sub Bus.Channel
  deriving (Show)

newRequest :: Handle -> IO (Either Text Request)
newRequest conn = do
  op <- Text.strip <$> TIO.hGetLine conn
  case op of
    "PUB" -> do
      channel <- Text.strip <$> TIO.hGetLine conn
      message <- Text.strip <$> TIO.hGetLine conn
      return $ Right $ Pub channel message
    "SUB" -> do
      channel <- Text.strip <$> TIO.hGetLine conn
      return $ Right $ Sub channel
    _ -> return $ Left $ Text.append "Unknown operation: " op
