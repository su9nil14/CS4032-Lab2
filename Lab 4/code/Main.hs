module Main where

import Server
import Network
import Control.Exception
import System.Environment

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    startServer $ head args
