module Main where

import Interpret
import Parse
import Value
import System.IO
import System.Environment

import Control.Applicative

main = do
    args <- getArgs
    if null args
    then putStrLn "No input file."
    else do
        let filename = head args
        handle <- openFile filename ReadMode
        code <- hGetContents handle
        (RVal _ x, _) <- run (parse code) "main" []
        putStr x

parseNrun s = do
    (v, _) <- run (parse s) "main" []
    print v
