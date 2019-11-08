module Main where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM   (STM, TQueue)
import qualified Control.Concurrent.STM   as STM
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import qualified Data.Text                as Text

type Channel = Text

type Message = Text

type Bus = STM.TVar (Map Channel [STM.TQueue Message])

subscribe :: Bus -> Channel -> STM (STM.TQueue Message)
subscribe bus channel = do
  queue <- STM.newTQueue
  STM.stateTVar bus (insert queue)
  where
    insert queue bus = (queue, Map.insertWith (++) channel [queue] bus)

publish :: Bus -> Channel -> Message -> STM ()
publish bus channel message = do
  bus' <- STM.readTVar bus
  mapM_ (mapM_ $ write message) $ Map.lookup channel bus'
  where
    write message = flip STM.writeTQueue message

main :: IO ()
main = putStrLn "hello"
