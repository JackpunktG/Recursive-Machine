module Main where

import Sequences.Fibonacci
import System.Environment
import Control.Exception
import Data.Time.Clock
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import Numeric
import Control.DeepSeq

{-
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("Hashmap", setHashMap)
           , ("Array", setArray)
           , ("Boosted", boosted)
           , ("Basic", basic)
           ]
-}

main :: IO ()
main = do
    (fileName:_) <- getArgs
    nS <- getLine
    let n = read nS :: Int
    writeFile fileName ("Hashmap test to " ++ show n ++ "\n")
    loopHashMap n fileName
    
    
loopHashMap :: Int -> String -> IO ()
loopHashMap 1000000 fileName = do 
    start <- getCurrentTime
    let result = evalState (fibHashMap 1000000) HM.empty
    result `deepseq` return () --forces evaluation
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start) :: Float
    appendFile fileName ("1000000," ++ show result ++ "," ++  (showFFloat (Just 8) diff "") ++ "\n")
loopHashMap n fileName = do
    start <- getCurrentTime
    let result = evalState (fibHashMap n) HM.empty
    result `deepseq` return () --forces evaluation
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start) :: Float
    appendFile fileName (show n ++ "," ++ show result ++ "," ++ (showFFloat (Just 8) diff "") ++ "\n")
    loopHashMap (n-1) fileName
    
    






    
    
{-
generateHashMap :: Int -> HM.HashMap Int Int
generateHashMap n = HM.fromList [(n, fibSeq'' n) | n <- [0..n]]


fibSeq'' :: Int -> HashMap -> Int
fibSeq'' 0 _ = 0
fibSeq'' 1 _ = 1
fibSeq'' n _ = if HM.lookup n /= 0 then HM.lookup n :: Int else fibSeq'' (n-1) + fibSeq'' (n-2)
-}