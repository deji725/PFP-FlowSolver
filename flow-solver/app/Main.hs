module Main where

import Lib

import System.IO(readFile)
import System.Environment(getArgs)
import Data.Maybe
import Data.Char(isUpper)
import Data.Vector((!?),(!))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S


type Board = (V.Vector (V.Vector Char))

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let ls = lines contents -- line = "00gh0"
  let v = V.fromList (head ls)
  let matrix = V.fromList $ map V.fromList ls
  let colors = S.delete '0' $ S.fromList $ concat ls
  let ends = M.delete '0' $ getEnds matrix
  print $ getEnds matrix
  print  $ isSolved matrix colors ends
  return ()
  {-
   - nxt_color = color with min moves left
   - solve index:
   -      for all possible moves in (neighbor_idx nxt_color)
   -          cur = solve nxt_idx
   -          if isSolved cur : return cur
  -}
  -- mapM_ print v
solver :: Board -> M.Map Char [(Int,Int)] -> Maybe Board
solver board ends = helper board ends
  where 
        helper board fronts = Nothing 
          where
                possible_moves = M.foldl (foldl (\m pos -> M.insert pos (getMoves pos) m)) M.empty fronts  
                --getMoves :: (Int,Int) -> [((Int,Int),Char)]
                getMoves (i,j) =  map fst $ filter (\(pos, ch) -> (ch == '0' || ch == cur_char) ) $ neighbors_idxs  (i,j) board
                 where cur_char = board ! i ! j


isSolved :: Board -> S.Set Char -> M.Map Char [(Int,Int)] -> Bool
isSolved board colors ends
    | V.any (\v -> V.any (not . isUpper) v) board = False -- all places filled
    | otherwise = all color_has_path colors
    where color_has_path c = validPath board strt end
            where (strt:end:_) = ends M.! c

getEnds :: Board -> M.Map Char [(Int,Int)]
getEnds board = foldl (helper) M.empty [0.. (V.length $ board)-1] 
  where helper boardMap i = foldl (helper2) boardMap [0.. V.length (board ! i) - 1]
          where helper2 m j = 
                  if is_end then 
                    M.insertWith (++) (board ! i ! j) [(i,j)] m
                  else
                    m
                  where 
                    is_end = (length $ filter ((==) (board ! i !? j)) (neighbors (i,j) board)) <= 1

validPath :: Board -> (Int,Int) -> (Int,Int) -> Bool
validPath board (i,j) end = helper (fst $ head first_step) (i,j) 
  where 
        cur_char = board ! i ! j
        first_step = filter (\(_,c) -> c == cur_char) (neighbors_idxs (i,j) board)
        helper cur_pos p  
                | cur_pos == end = True
                | length nextStep /= 1 = False
                | otherwise = helper (fst $ head nextStep) cur_pos 
          where nextStep = 
                  filter (\(idx, c) -> (c == cur_char) && idx /= p) (neighbors_idxs cur_pos board)



neighbors_idxs :: (Int,Int) -> Board -> [((Int,Int), Char)]
neighbors_idxs (i,j) board = map (\(p,m) -> (p, fromJust m)) $ filter (\a -> isJust (snd a)) tmp
 where tmp = zip ([(i, j-1), (i-1, j), (i,j+1), (i+1,j)]) (neighbors (i,j) board)

-- returns the neigbors of the color at (i,j)
neighbors :: (Int,Int) -> Board -> [Maybe Char]
neighbors (i,j) board = [left, up, right, down]
  where left = board  ! i !? (j-1)
        right = board ! i !? (j+1)
        up = case board !? (i-1) of
               Nothing -> Nothing
               Just row -> row !? j
        down = case board !? (i+1) of
               Nothing -> Nothing
               Just row -> row !? j
