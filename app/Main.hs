module Main where

import Lisp

import System.Environment (getArgs)

main :: IO ()
main = do
    filename <- head <$> getArgs
    file <- readFile filename
    res <- runmain filename file
    putStr res
