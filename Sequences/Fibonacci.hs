module Sequences.Fibonacci
( fibonacci,
  fibArray,
  fibHashMap,
  fibSeq
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
fibArray n = array (0, n) $  
    [ (i, fib i) | i <- [0..n] ]
  where
    fib 0 = 0
    fib 1 = 1
    fib i = fibArray n ! (i - 1) + fibArray n ! (i - 2)


-- my "improved" self-recursive function
fibonacci :: Int -> Integer
fibonacci n 
    | n < 20 = fibSeq n
    | n < 40 = fibSeq' n18 18 n19 19 n
    | n < 60 = fibSeq' n38 38 n39 39 n
    | n < 80 = fibSeq' n58 58 n59 59 n
    | n < 100 = fibSeq' n78 78 n79 79 n
    | n < 120 = fibSeq' n98 98 n99 99 n
    | n < 140 = fibSeq' n118 118 n119 119 n
    | n < 160 = fibSeq' n138 138 n139 139 n
    | n < 180 = fibSeq' n158 158 n159 159 n
    | n < 200 = fibSeq' n178 178 n179 179 n
    | n >= 200 = fibSeq' n198 198 n199 199 n
    where
        n18  = fibonacci 18;  n19  = fibonacci 19
        n38  = fibonacci 38;  n39  = fibonacci 39
        n58  = fibonacci 58;  n59  = fibonacci 59
        n78  = fibonacci 78;  n79  = fibonacci 79
        n98  = fibonacci 98;  n99  = fibonacci 99
        n118 = fibonacci 118; n119 = fibonacci 119
        n138 = fibonacci 138; n139 = fibonacci 139
        n158 = fibonacci 158; n159 = fibonacci 159
        n178 = fibonacci 178; n179 = fibonacci 179
        n198 = fibonacci 198; n199 = fibonacci 199
       

fibSeq' :: Integer -> Int -> Integer -> Int -> Int -> Integer
fibSeq' nn8 xx8 nn9 xx9 n
    | n == xx8 = nn8
    | n == xx9 = nn9
fibSeq' nn8 xx8 nn9 xx9 n = (fibSeq' nn8 xx8 nn9 xx9 (n-1)) + (fibSeq' nn8 xx8 nn9 xx9 (n-2))
  
 
fibSeq :: Int -> Integer
fibSeq 0 = 0
fibSeq 1 = 1
fibSeq n = fibSeq (n-1) + fibSeq (n-2)