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