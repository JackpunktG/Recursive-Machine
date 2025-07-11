--runhaskell --ghc-arg=-package --ghc-arg=time --ghc-arg=-package --ghc-arg=deepseq --ghc-arg=-package --ghc-arg=process --ghc-arg=-package --ghc-arg=array --ghc-arg=-package --ghc-arg=unordered-containers --ghc-arg=-package --ghc-arg=mtl FibMain.hs

import Data.Char
import System.Environment
import System.Process


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
    
    writeFile fileName ("Hasshmap memoization test from " ++ show hashL ++ " to " ++ show hashU ++ "\n")
    loopTest "hashmap" (hashU-1) (hashL-1) fileName
    appendFile fileName ("\n\nArray memoization Test to " ++ show arrayL ++ " to " ++ show arrayU ++ "\n")
    loopTest "array" (arrayU-1) (arrayL-1) fileName
    appendFile fileName ("\n\nList memoization Test to " ++ show listL ++ " to " ++ show listU ++ "\n")
    loopTest "list" (listU-1) (listL-1) fileName
    appendFile fileName ("\n\nBoosted recursive function Test to " ++ show boostL ++ " to " ++ show boostU ++ "\n")
    loopTest "boosted" (boostU-1) (boostL-1) fileName
    appendFile fileName ("\n\nBasic recursive function Test to " ++ show basicL ++ " to " ++ show basicU ++  "\n")
    loopTest "basic" (basicU-1) (basicL-1) fileName
    
    
    
loopTest :: String -> Int -> Int -> String -> IO String
loopTest function upper lower fileName 
    | upper == lower = runTest (function ++ " " ++ fileName ++ " " ++ show lower) ghcArgs
    | otherwise = do
        runTest (function ++ " " ++ fileName ++ " " ++ show lower) ghcArgs 
        loopTest function upper (lower+1) fileName
    
runTest :: String -> [String] -> IO String
runTest str args = readProcess "runhaskell" (args ++ words str) ""

ghcArgs :: [String]
ghcArgs =
  [ "--ghc-arg=-package"
  , "--ghc-arg=time"
  , "--ghc-arg=-package"
  , "--ghc-arg=deepseq"
  , "--ghc-arg=-package"
  , "--ghc-arg=process"
  , "--ghc-arg=-package"
  , "--ghc-arg=array"
  , "--ghc-arg=-package"
  , "--ghc-arg=unordered-containers"
  , "--ghc-arg=-package"
  , "--ghc-arg=mtl"
  , "FibMain.hs"
  ]