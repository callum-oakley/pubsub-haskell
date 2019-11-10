{-# LANGUAGE OverloadedStrings #-}

module Request
  ( Request(..)
  , newRequest
  ) where

import           Control.Monad.Trans.Except (ExceptT (..))
import qualified Control.Monad.Trans.Except as Except
import           System.IO                  (Handle)

import qualified Bus
import           Error                      (Error)
import qualified Error
import           ExceptTErrorIO

data Request
  = Pub Bus.Channel Bus.Message
  | Sub Bus.Channel
  deriving (Show)

newRequest :: Handle -> ExceptT Error IO Request
newRequest conn = do
  op <- hGetLine conn
  case op of
    "PUB" -> do
      channel <- hGetLine conn
      message <- hGetLine conn
      return $ Pub channel message
    "SUB" -> do
      channel <- hGetLine conn
      return $ Sub channel
    _ -> Except.throwE $ Error.UnknownOperation op
