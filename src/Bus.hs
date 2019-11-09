module Bus
  ( Bus
  , Channel
  , Message
  , newBus
  , publish
  , subscribe
  ) where

import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)

type Channel = Text

type Message = Text

type Bus = STM.TVar (Map Channel [STM.TQueue Message])

newBus :: STM Bus
newBus = STM.newTVar Map.empty

publish :: Bus -> Channel -> Message -> STM ()
publish bus channel message = do
  bus' <- STM.readTVar bus
  (mapM_ . mapM_ $ write message) $ Map.lookup channel bus'
  where
    write = flip STM.writeTQueue

-- TODO mechanism for unsubscribing
subscribe :: Bus -> Channel -> STM (STM.TQueue Message)
subscribe bus channel = do
  queue <- STM.newTQueue
  STM.stateTVar bus $ insert queue
  where
    insert queue b = (queue, Map.insertWith (++) channel [queue] b)
