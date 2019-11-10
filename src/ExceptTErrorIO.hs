{-# LANGUAGE OverloadedStrings #-}

module ExceptTErrorIO
  ( hGetLine
  , hPutLines
  , putLine
  ) where

import qualified Control.Monad.Trans        as MTrans
import           Control.Monad.Trans.Except (ExceptT (..))
import qualified Control.Monad.Trans.Except as Except
import qualified Data.List                  as List
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TIO
import           System.IO                  (Handle)
import qualified System.IO.Error            as IOError

import           Error                      (Error)
import qualified Error

hGetLine :: Handle -> ExceptT Error IO Text
hGetLine =
  Except.withExceptT Error.IOError .
  ExceptT . IOError.tryIOError . fmap Text.strip . TIO.hGetLine

hPutLines :: Handle -> [Text] -> ExceptT Error IO ()
hPutLines conn ts =
  Except.withExceptT Error.IOError $
  ExceptT $
  IOError.tryIOError $
  TIO.hPutStr conn $
  Text.append (Text.concat $ List.intersperse "\r\n" ts) "\r\n"

putLine :: [Text] -> ExceptT Error IO ()
putLine ts =
  MTrans.lift $
  TIO.putStr $ Text.append (Text.concat $ List.intersperse " " ts) "\n"
