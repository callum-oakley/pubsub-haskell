{-# LANGUAGE LambdaCase #-}

module Bus
  ( Bus
  , Channel
  , Message
  , newBus
  , publish
  , subscribe
  , debugReport
  ) where

import           Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)

type Channel = Text

type Message = Text

data Bus =
  Bus
    { subscriptions :: STM.TVar (Map Channel (Map Integer (STM.TQueue Message)))
    , nextSubID     :: STM.TVar Integer
    }

newBus :: STM Bus
newBus = do
  subs <- STM.newTVar Map.empty
  subID <- STM.newTVar 0
  return Bus {subscriptions = subs, nextSubID = subID}

publish :: Bus -> Channel -> Message -> STM ()
publish bus channel message = do
  subs <- STM.readTVar $ subscriptions bus
  (mapM_ . mapM_ $ write message) $ Map.lookup channel subs
  where
    write = flip STM.writeTQueue

subscribe :: Bus -> Channel -> STM (STM.TQueue Message, STM ())
subscribe bus channel = do
  sub <- STM.newTQueue
  subID <- STM.readTVar $ nextSubID bus
  STM.modifyTVar (nextSubID bus) succ
  STM.modifyTVar (subscriptions bus) (insert subID sub)
  return (sub, remove subID)
  where
    insert subID sub =
      Map.alter
        (\case
           Just subs -> Just $ Map.insert subID sub subs
           Nothing -> Just $ Map.fromList [(subID, sub)])
        channel
    remove subID =
      STM.modifyTVar (subscriptions bus) (Map.adjust (Map.delete subID) channel)

debugReport :: Bus -> IO ()
debugReport bus = do
  bus' <- STM.atomically $ STM.readTVar $ subscriptions bus
  putStr $ "#subscriptions: " ++ show (sum $ fmap length bus') ++ "\n"
