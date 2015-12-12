

module Semaphore where

import Control.Concurrent.MVar

newtype Semaphore = Semaphore (MVar Int)

newSemaphore :: Int -> IO Semaphore
newSemaphore i = do
    m <- newMVar i
    return (Semaphore m)

checkSemaphore :: Semaphore -> IO Bool
checkSemaphore (Semaphore m) =
    modifyMVar m $ \i ->
        if i == 0 then return (i, False)
        else let z = i-1 in return (z, True)

signalSemaphore :: Semaphore -> IO ()
signalSemaphore (Semaphore m) =
    modifyMVar m $ \i ->
        let z = i+1 in return (z, ())