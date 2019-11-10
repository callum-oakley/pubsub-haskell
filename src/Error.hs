module Error
  ( Error(..)
  ) where

import           Data.Text       (Text)
import           System.IO.Error (IOError)

data Error
  = UnknownOperation Text
  | IOError IOError
  deriving (Show)
