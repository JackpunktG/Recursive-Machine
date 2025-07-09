module Main where

import Sequences.Fibonacci
import System.Environment
import Control.Exception
import Data.Time.Clock
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import Data.Array
import Numeric
import Control.DeepSeq



main :: IO ()
main = do
    putStrLn "Hashmap upper bound:"
    hashUpper <- getLine
    putStrLn "Hashmap lower bound:"
    hashLower <- getLine 
    putStrLn "List upper bound:"
    listUpper <- getLine
    putStrLn "List Lower bound:"
    listLower <- getLine
    putStrLn "Array upper bound:"
    arrayUpper <- getLine 
    putStrLn "Array lower bound:"
    arrayLower <- getLine
    putStrLn "Boosted upper bound:"
    boostUpper <- getLine 
    putStrLn "Boosted lower bound:"
    boostLower <- getLine
    putStrLn "Basic upper bound:"
    basicUpper <- getLine 
    putStrLn "Basic lower bound:"
    basicLower <- getLine
    let hashU = read hashUpper :: Int
        hashL = read hashLower :: Int
        listU = read listUpper :: Int
        listL = read listLower :: Int
        arrayU = read arrayUpper :: Int
        arrayL = read arrayLower :: Int
        boostU = read boostUpper :: Int
        boostL = read boostLower :: Int
        basicU = read basicUpper :: Int
        basicL = read basicLower :: Int
    putStrLn "Filename?"
    fileName <- getLine 
    
    writeFile fileName ("Hasshmap memoization test from " ++ show hashU ++ " to " ++ show hashL ++ "\n")
    loopHashMap (hashU-1) (hashL-1) fileName
    appendFile fileName ("\n\nArray memoization Test to " ++ show arrayU ++ " to " ++ show arrayL ++ "\n")
    loopArray (arrayU-1) (arrayL-1) fileName
    appendFile fileName ("\n\nList memoization Test to " ++ show listU ++ " to " ++ show listL ++ "\n")
    loopList (listU-1) (listL-1) fileName
    appendFile fileName ("\n\nBoosted recursive function Test to " ++ show boostU ++ " to " ++ show boostL ++ "\n")
    loopBoosted (boostU-1) (boostL-1) fileName
    appendFile fileName ("\n\nBasic recursive function Test to " ++ show basicU ++ " to " ++ show basicL ++  "\n") 
    loopFib (basicU-1) (basicL-1) fileName


loopFib :: Int -> Int -> String -> IO ()
loopFib upper lower fileName 
    | upper == lower = do
        start <- getCurrentTime
        let result = fibSeq lower
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (lower+1) ++ "," ++ show result ++ "," ++  (showFFloat (Just 8) diff "") ++ "\n")
    | otherwise = do
        start <- getCurrentTime
        let result = fibSeq upper
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (upper+1) ++ "," ++ show result ++ "," ++ (showFFloat (Just 8) diff "") ++ "\n")
        loopFib (upper-1) lower fileName
    

loopBoosted :: Int -> Int -> String -> IO ()
loopBoosted upper lower fileName
    | upper == lower = do 
        start <- getCurrentTime
        let result = fibboosted lower
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (lower+1) ++ "," ++ show result ++ "," ++  (showFFloat (Just 8) diff "") ++ "\n")
    | otherwise = do
        start <- getCurrentTime
        let result = fibboosted upper
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (upper+1) ++ "," ++ show result ++ "," ++ (showFFloat (Just 8) diff "") ++ "\n")
        loopBoosted (upper-1) lower fileName
   
    
loopHashMap :: Int -> Int -> String -> IO ()
loopHashMap upper lower fileName 
    | lower == upper = do 
        start <- getCurrentTime
        let result = evalState (fibHashMap lower) HM.empty
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (lower+1) ++ "," ++ show result ++ "," ++  (showFFloat (Just 8) diff "") ++ "\n")
    | otherwise = do
        start <- getCurrentTime
        let result = evalState (fibHashMap upper) HM.empty
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (upper+1) ++ "," ++ show result ++ "," ++ (showFFloat (Just 8) diff "") ++ "\n")
        loopHashMap (upper-1) lower fileName
    
    
loopArray :: Int -> Int -> String -> IO()
loopArray upper lower fileName 
    | upper == lower = do
        start <- getCurrentTime
        let result = (fibArray lower) ! lower
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (lower+1) ++ "," ++ show result ++ "," ++  (showFFloat (Just 8) diff "") ++ "\n")
    | otherwise = do
        start <- getCurrentTime
        let resultAraay = fibArray upper
            result = resultAraay ! upper
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (upper+1) ++ "," ++ show result ++ "," ++  (showFFloat (Just 8) diff "") ++ "\n")
        loopArray (upper-1) lower fileName


loopList :: Int -> Int -> String -> IO()
loopList upper lower fileName
    | upper == lower = do
        start <- getCurrentTime
        let result = fibZip !! lower
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (lower+1) ++ "," ++ show result ++ "," ++  (showFFloat (Just 8) diff "") ++ "\n")
    | otherwise = do
        start <- getCurrentTime
        let result = fibZip !! upper
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (upper + 1) ++ "," ++ show result ++ "," ++  (showFFloat (Just 8) diff "") ++ "\n")
        loopList (upper-1) lower fileName


