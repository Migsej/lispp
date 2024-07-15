module Main where

import Data.List (groupBy, isSuffixOf, sort, sortBy)
import Data.List.Split
import Lisp
import System.Directory
import qualified System.Exit as Exit
import Test.HUnit

tozip :: [[String]] -> [(String, String)]
tozip [xs, ys]
    | "excpect" `isSuffixOf` head xs = zip xs ys
    | otherwise = zip ys xs
tozip x = error $ show x

runTest :: (String, String) -> IO Test
runTest (expect, lisp) = do
    expectedoutput <- readFile ("testcases/" ++ expect)
    lispfile <- readFile ("testcases/" ++ lisp)
    result <- runmain lisp lispfile
    return $ TestCase $ assertEqual lisp expectedoutput result

main :: IO ()
main = do
    dir <- getDirectoryContents "testcases/"
    let cases = tozip $ groupBy (\x y -> last (splitOn "." y) == last (splitOn "." x)) $ sortBy (\a b -> compare (reverse a) (reverse b)) $ filter (\x -> ("excpect" `isSuffixOf` x) || ("lisp" `isSuffixOf` x)) dir
    tests <- traverse runTest cases
    result <- runTestTT $ TestList tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
