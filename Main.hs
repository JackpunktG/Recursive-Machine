module Main where

import Sequences.Fibonacci
import System.Environment
import Data.Array
import qualified Data.HashMap.Strict as HM
import Control.Monad.State

main :: IO ()
main = do
    putStrLn "Enter n:"
    input <- getLine
    let n = read input :: Int
    let result = evalState (fibHashMap n)  HM.empty
       -- fibs = fibArray n
    --mapM_ (print . (fibs !)) [0..n]
    print result





    
    
{-


generateHashMap :: Int -> HM.HashMap Int Int
generateHashMap n = HM.fromList [(n, fibSeq'' n) | n <- [0..n]]


fibSeq'' :: Int -> HashMap -> Int
fibSeq'' 0 _ = 0
fibSeq'' 1 _ = 1
fibSeq'' n _ = if HM.lookup n /= 0 then HM.lookup n :: Int else fibSeq'' (n-1) + fibSeq'' (n-2)
-}