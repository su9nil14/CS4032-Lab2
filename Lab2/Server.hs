
module Main where

import Network
import Network.BSD
import Control.Concurrent
import Control.Exception (try)
import Control.Monad
import System.IO
import System.Exit
import System.Environment

maxConnections :: Int
maxConnections = 200

-- Semaphore for thread pooling

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

-- Server

startServer :: Int -> IO ()
startServer port = do
    putStrLn $ "Listening on port " ++ show port ++ "..."
    sock <- listenOn $ PortNumber (fromIntegral port)
    sem <- newSemaphore maxConnections
    host <- getFQDN
    acceptConnections sock host port sem

acceptConnections :: Socket -> HostName -> Int -> Semaphore -> IO ()
acceptConnections sock host port sem = do
    res <- try $ accept sock :: IO (Either IOError (Handle, HostName, PortNumber))
    case res of
        Left _ -> do
            putStrLn "Terminating..."
            exitSuccess
        Right (handle, _, _) -> do
            hSetBuffering handle NoBuffering

            canAquireSem <- checkSemaphore sem
            if canAquireSem then do
                void $ forkIO $ processRequest sock handle host port sem
                acceptConnections sock host port sem
            else do
                hPutStrLn handle "SERVER_BUSY"
                hClose handle
                acceptConnections sock host port sem

processRequest :: Socket -> Handle -> HostName -> Int -> Semaphore -> IO ()
processRequest sock handle host port sem = do
    message <- hGetLine handle
    putStrLn $ "[" ++ host ++ ":" ++ show port ++ "]" ++ " " ++ message

    case head $ words message of
        "HELO" -> hPutStr handle $ buildHELOResponse message host port
        "KILL_SERVICE" -> hPutStr handle message >> sClose sock
        _ -> putStrLn $ "Unknown Command:" ++ message

    hClose handle
    signalSemaphore sem

getFQDN :: IO HostName
getFQDN = liftM hostName (getHostName >>= getHostByName)

buildHELOResponse :: String -> HostName -> Int -> String
buildHELOResponse message host port = message ++ "\n" ++
                                      "IP:" ++ host ++ "\n" ++
                                      "Port:" ++ show port ++ "\n" ++
                                      "StudentID:1234567"

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = read $ head args :: Int
    startServer port
