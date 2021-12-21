module Main where

import Lib

import System.Environment(getArgs)
import System.Exit(die)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  (filename, parallelize, depth) <-
    case args of 
      [fn, p] -> return (fn,p,10) -- default depth of 10
      [fn, p, d] -> return (fn,p, read d :: Int)
      _   -> die "Usage: flow-solver [filename] [par|seq]"
  let solver = if parallelize == "par" then par_solver depth else seq_solver

  contents <- readFile filename
  let ls = lines contents
  let matrix = V.fromList $ map V.fromList ls
  let colors = S.delete '0' $ S.fromList $ concat ls
  let ends = M.delete '0' $ getEnds matrix
  let sol = solver matrix colors ends
  case sol of 
    Nothing -> putStrLn "The board does not have a solution"
    Just s  -> putStrLn $ myShow s

