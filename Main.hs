module Main where

import Sequences.Fibonacci


main :: IO ()
main = do
    n <- getLine
    putStrLn $ show (fibonacci (read n :: Integer))