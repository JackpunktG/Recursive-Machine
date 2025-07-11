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
    (command:fileName:number:_) <- getArgs  
    let n = read number :: Int
    start <- getCurrentTime
    let result = fibChooser command n
    result `deepseq` return ()   
    end <- getCurrentTime
    let diff = realToFrac (diffUTCTime end start) :: Float
    appendFile fileName (show (n+1) ++ "," ++ show result ++ "," ++ (showFFloat (Just 8) diff "") ++ "\n")
    
    
    
fibChooser :: String -> Int -> Integer
fibChooser function n 
    | function == "hashmap" = evalState (fibHashMap n) HM.empty
    | function == "array" = (fibArray n) ! n
    | function == "list" = fibZip !! n
    | function == "boosted" = fibboosted n
    | function == "basic" = fibSeq n
    | otherwise = error "Not Valid function"


{-
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
        let result = freshFibBoosted lower
        result `deepseq` return () --forces evaluation
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (lower+1) ++ "," ++ show result ++ "," ++  (showFFloat (Just 8) diff "") ++ "\n")
    | otherwise = do
        start <- getCurrentTime
        let result = freshFibBoosted upper
        result `deepseq` return () --forces evaluation!
        end <- getCurrentTime
        let diff = realToFrac (diffUTCTime end start) :: Float
        appendFile fileName (show (upper+1) ++ "," ++ show result ++ "," ++ (showFFloat (Just 8) diff "") ++ "\n")
        loopBoosted (upper-1) lower fileName
   
    
loopHashMap :: Int -> Int -> String -> IO ()
loopHashMap upper lower fileName 
    | lower == upper = do 
        start <- getCurrentTime
        let result = evalState (fibHashMap lower) HM.empty
        result `deepseq` return () --forces evaluation!
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


-}