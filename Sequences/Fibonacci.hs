module Sequences.Fibonacci
( fibboosted,
  fibArray,
  fibHashMap,
  fibSeq,
  fibZip
) where

import Data.Array
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Control.Monad.State





fibHashMap :: Int -> State (HashMap Int Integer) Integer
fibHashMap 0 = return 0
fibHashMap 1 = return 1
fibHashMap n = do
    memo <- get  --get gets the current state of the HashMap and binds it to memo
    case HM.lookup n memo of
        Just value -> return value  -- if value exists return it
        Nothing -> do
            a <- fibHashMap (n - 1)
            b <- fibHashMap (n - 2)
            let result = a + b
            modify (HM.insert n result)  -- update the HashMap in state
            return result
            
            
-- Array memoization of Fibonacci
fibArray :: Int -> Array Int Integer
fibArray n = arr
  where
    arr = array (0, n) [(i, fib i) | i <- [0..n]]
    fib 0 = 0
    fib 1 = 1
    fib i = arr ! (i - 1) + arr ! (i - 2)
    
    
    
-- Ziptail memoization 
fibZip :: [Integer]
fibZip = 0 : 1 : zipWith (+) fibZip (tail fibZip)

-- my "boosted" self-recursive function
fibboosted :: Int -> Integer
fibboosted n
  | n < 2 = fromIntegral n
  | otherwise = fibSeq' (n - mod n 20) (fibboosted (n - mod n 20)) (fibboosted (n - mod n 20 + 1)) n

       
-- helper function to build the pairs dynamically with the boosted recursive function
fibSeq' :: Int -> Integer -> Integer -> Int -> Integer
fibSeq' k fk1 fk2 n
  | k == n    = fk1
  | k + 1 == n = fk2
  | otherwise = fibSeq' (k + 1) fk2 (fk1 + fk2) n 
  
  
--most basic Fibonnaci recursive function
fibSeq :: Int -> Integer
fibSeq 0 = 0
fibSeq 1 = 1
fibSeq n = fibSeq (n-1) + fibSeq (n-2)