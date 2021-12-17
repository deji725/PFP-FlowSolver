module Main where

import Lib

import System.IO(readFile, hPutStrLn)
import System.Environment(getArgs)
import Data.Vector((!?))
import qualified Data.Vector as V

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let ls = lines contents -- line = "00gh0"
  let v = V.fromList (head ls)
  let matrix = V.fromList $ map V.fromList ls
  print v
  print $ v !? 7
  print $ v !? 2
  -- mapM_ print v

-- someFunc
