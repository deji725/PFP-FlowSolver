module Main where

import Lib

import System.IO(readFile, hPutStrLn)
import System.Environment(getArgs)
import Data.Vector((!?),(!))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M


type Board = (V.Vector (V.Vector Char))

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let ls = lines contents -- line = "00gh0"
  let v = V.fromList (head ls)
  let matrix = V.fromList $ map V.fromList ls
  print  $ isSolverHelper matrix
  return ()
  -- mapM_ print v

isSolved :: Board -> Bool
isSolved board = True

isSolverHelper :: Board -> M.Map Char [(Int,Int)]
isSolverHelper board = foldl (helper) M.empty [0.. (V.length $ board)-1] 
    where helper boardMap i = foldl (helper2) boardMap [0.. V.length (board ! i) - 1]
	    where helper2 boardMap j =  M.insertWith (++) (board ! i ! j) [(i,j)] boardMap

