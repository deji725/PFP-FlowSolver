module Main where

import Lib

import System.IO(readFile)
import System.Environment(getArgs)
import Data.Maybe
import Data.Char(isUpper, toUpper)
import Data.Vector((!?),(!), (//))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S


type Board = (V.Vector (V.Vector Char))
type Fronts = M.Map Char [Pos]
type Ends   = Fronts
type Pos    = (Int,Int)

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let ls = lines contents -- line = "00gh0"
  let v = V.fromList (head ls)
  let matrix = V.fromList $ map V.fromList ls
  let colors = S.delete '0' $ S.fromList $ concat ls
  let ends = M.delete '0' $ getEnds matrix
  -- print  $ isSolved matrix colors ends
  print $ solver matrix colors ends
  return ()
  {-
   - nxt_color = color with min moves left
   - solve index:
   -      for all possible moves in (neighbor_idx nxt_color)
   -          cur = solve nxt_idx
   -          if isSolved cur : return cur
  -}
  -- mapM_ print v
solver :: Board -> S.Set Char -> M.Map Char [Pos] -> Maybe Board
solver board colors ends  = helper board ends
  where 
        helper cur_board fronts 
          | isSolved cur_board colors ends = Just cur_board 
          | M.size nextMoves == 0 = Nothing
          | otherwise = case filter isJust sub_sols of
                          [] -> Nothing
                          (s:_) -> s
          where
            nextMoves = getNextMoves cur_board fronts
            (best_pos@(i,j), moves) = getShortestMove nextMoves
            best_char = cur_board ! i ! j
            sub_sols = map (\nxt -> helper 
                                      (makeMove cur_board best_pos nxt) 
                                      (advanceFront fronts best_char best_pos nxt)) 
                        moves


advanceFront :: Fronts -> Char -> Pos -> Pos -> Fronts
advanceFront fronts c old new = 
  M.insert c (new:(filter ((/=) old) $ fronts M.! c)) fronts

makeMove :: Board -> Pos -> Pos -> Board
makeMove board (b1,b2) (a1,a2) = replaceBoard (b1,b2) tmp (toUpper cur_char)
  where cur_char = board ! b1 ! b2
        replaceBoard (i,j) brd value = brd // [(i, brd ! i // [(j, value)])]
        tmp = replaceBoard (a1,a2) board cur_char



getShortestMove :: M.Map Pos [Pos] -> (Pos, [Pos])
getShortestMove all_moves = 
  M.foldlWithKey getMinMoves ((0,0), replicate 5 (-1,-1)) all_moves
    where getMinMoves cur_min@(_, min_moves) cur_pos cur_moves = 
            if length min_moves > length cur_moves then (cur_pos, cur_moves) 
            else cur_min


-- Gets all the possible moves on the board
getNextMoves :: Board -> Fronts -> M.Map Pos [Pos]
getNextMoves board fronts = 
  M.foldl (foldl (\m pos -> M.insert pos (getMoves pos) m)) M.empty fronts  
  where getMoves (i,j) =  map fst $ --getMoves :: Pos -> [(Pos,Char)]
                      filter (\(_, ch) -> (ch == '0' || ch == cur_char) ) 
                      $ neighbors_idxs  (i,j) board
          where cur_char = board ! i ! j

isSolved :: Board -> S.Set Char -> M.Map Char [Pos] -> Bool
isSolved board colors ends
    | V.any (\v -> V.any (not . isUpper) v) board = False -- all places filled
    | otherwise = all color_has_path colors
    where color_has_path c = validPath board strt end
            where (strt:end:_) = ends M.! c

getEnds :: Board -> M.Map Char [Pos]
getEnds board = foldl (helper) M.empty [0.. (V.length $ board)-1] 
  where helper boardMap i = foldl (helper2) boardMap [0.. V.length (board ! i) - 1]
          where helper2 m j = 
                  if is_end then 
                    M.insertWith (++) (board ! i ! j) [(i,j)] m
                  else
                    m
                  where 
                    is_end = (length $ filter ((==) (board ! i !? j)) (neighbors (i,j) board)) <= 1

validPath :: Board -> Pos -> Pos -> Bool
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



neighbors_idxs :: Pos -> Board -> [(Pos, Char)]
neighbors_idxs (i,j) board = map (\(p,m) -> (p, fromJust m)) $ filter (\a -> isJust (snd a)) tmp
 where tmp = zip ([(i, j-1), (i-1, j), (i,j+1), (i+1,j)]) (neighbors (i,j) board)

-- returns the neigbors of the color at (i,j)
neighbors :: Pos -> Board -> [Maybe Char]
neighbors (i,j) board = [left, up, right, down]
  where left = board  ! i !? (j-1)
        right = board ! i !? (j+1)
        up = case board !? (i-1) of
               Nothing -> Nothing
               Just row -> row !? j
        down = case board !? (i+1) of
               Nothing -> Nothing
               Just row -> row !? j
