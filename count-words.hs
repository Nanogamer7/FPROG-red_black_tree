import System.Random (randomRIO)
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import Control.DeepSeq()
import Data.List.Split()
import Data.Time.Clock()
import System.IO()
import Numeric

import Tokenizer
import RedBlack

-- for testing purposes
generateRandomList :: Int -> IO [Int]
generateRandomList n = mapM (const $ randomRIO (1, 100)) [1..n]

printTimeDifference :: String -> Integer -> Integer -> IO ()
printTimeDifference str t1 t2 = putStrLn $ str ++ ": " ++ showFFloat Nothing (fromIntegral (t2 - t1) / (10^12)) "" ++ "s"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      fullText <- readFile filePath
      time0 <- getCPUTime
      let outputFileName = "output.txt"
      writeFile outputFileName (unlines (map formatWord (inOrder (insertListWithCount (tokenize fullText) Empty))))
      time4 <- getCPUTime
      printTimeDifference "Total" time0 time4
      putStrLn $ "Word counts saved to: " ++ outputFileName
    _ -> putStrLn "Usage: <program> <input-file-path>"