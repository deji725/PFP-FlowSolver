module Lib where

import System.Environment(getArgs)
import System.Exit(die)
import Data.Maybe
import Data.List
import Data.Char(isUpper, toUpper)
import Data.Vector((!?),(!), (//))
import Control.Parallel.Strategies(using, parList, rseq)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Board = (V.Vector (V.Vector Char))
type Fronts = M.Map Char [Pos]
type Ends   = Fronts
type Pos    = (Int,Int)

myShow :: Board -> String
myShow board = concat $ intersperse "\n" $ V.toList $ V.map (V.toList) board

seq_solver :: Board -> S.Set Char -> M.Map Char [Pos] -> Maybe Board
seq_solver board colors ends  = helper board ends
  where 
        helper cur_board fronts 
          | isSolved cur_board colors ends = Just cur_board
          | M.size nextMoves == 0 = Nothing
          | length moves == 0 = helper (makeMove cur_board best_pos best_pos) 
                                  (M.delete best_char fronts)
          | otherwise = case filter isJust sub_sols of
                          [] -> Nothing
                          (s:_) -> s
          where
            nextMoves = getNextMoves cur_board fronts
            (best_pos@(i,j), moves) = getShortestMove nextMoves
            best_char = cur_board ! i ! j
            sub_problems = map (\nxt -> ((makeMove cur_board best_pos nxt), 
                                      (advanceFront fronts 
                                        best_char best_pos nxt)) 
                                       ) moves
            sub_sols = map (\(nxt_move, nxt_fronts) -> 
                              helper nxt_move nxt_fronts) sub_problems

par_solver :: Int -> Board -> S.Set Char -> M.Map Char [Pos] -> Maybe Board
par_solver depth board colors ends  = helper board ends depth
  where 
        helper cur_board fronts dpth
          | isSolved cur_board colors ends = Just cur_board
          | length sub_problems == 0 = Nothing
          | length sub_problems == 1 = let (b,f) = head sub_problems 
                                        in helper b f dpth
          | otherwise = case filter isJust sub_sols of
                          [] -> Nothing
                          (s:_) -> s
          where
            sub_problems = getSubProblems cur_board fronts
            sub_sols = 
              if dpth > 0 then 
                map helper_recurse sub_problems `using` parList rseq
              else 
                map helper_recurse sub_problems 
            helper_recurse (nxt_move,nxt_fronts) = 
              helper nxt_move nxt_fronts (dpth `quot` (length sub_problems))


-- maybe give back the length of all possible moves in the board if we make this
-- move
getSubSubProblemsSize :: [(Board, Fronts)] -> [Int]
getSubSubProblemsSize sub_problems =
  map (\(b,f) -> foldl (\s l -> s + (length l)) 0 (getNextMoves b f) ) 
    sub_problems
  -- map (\(b,f) -> length $ getSubProblems b f) sub_problems

getSubProblems :: Board -> Fronts -> [(Board, Fronts)]
getSubProblems board fronts 
  | length nextMoves == 0 = []
  | length moves == 0     = [(makeMove board best_pos best_pos, 
                                M.delete best_char fronts)]
  | otherwise = map (\nxt -> ((makeMove board best_pos nxt), 
                                (advanceFront fronts best_char best_pos nxt)) ) 
                  moves
  where nextMoves = getNextMoves board fronts
        (best_pos@(i,j), moves) = getShortestMove nextMoves
        best_char = board ! i ! j



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
  M.foldl helper M.empty fronts  
  where getMoves (i,j) =  map fst $ --getMoves :: Pos -> [(Pos,Char)]
                      filter (\(_, ch) -> (ch == '0' || ch == cur_char) ) 
                      $ neighbors_idxs  (i,j) board
          where cur_char = board ! i ! j
        helper m l = if all ((==) (head l)) l then M.insert (head l) [] m
                     else foldl (\m' pos -> M.insert pos (getMoves pos) m') m l

isSolved :: Board -> S.Set Char -> M.Map Char [Pos] -> Bool
isSolved board colors ends
    | V.any (\v -> V.any (not . isUpper) v) board = False -- all places filled
    | otherwise = all color_has_path colors
    where color_has_path c = validPath board strt end
            where (strt:end:_) = ends M.! c

getEnds :: Board -> M.Map Char [Pos]
getEnds board = foldl (helper) M.empty [0.. (V.length $ board)-1] 
  where helper boardMap i = foldl (helper2) boardMap 
                              [0.. V.length (board ! i) - 1]
          where helper2 m j = 
                  if is_end then 
                    M.insertWith (++) (board ! i ! j) [(i,j)] m
                  else
                    m
                  where 
                    is_end = (length $ filter ((==) (board ! i !? j)) 
                                (neighbors (i,j) board)) <= 1

validPath :: Board -> Pos -> Pos -> Bool
validPath board (i,j) end = helper (fst $ head first_step) (i,j) 
  where 
        cur_char = board ! i ! j
        first_step = filter (\(_,c) -> c == cur_char) 
                      (neighbors_idxs (i,j) board)
        helper cur_pos p  
                | cur_pos == end = True
                | length nextStep /= 1 = False
                | otherwise = helper (fst $ head nextStep) cur_pos 
          where nextStep = 
                  filter (\(idx, c) -> (c == cur_char) && idx /= p) 
                      (neighbors_idxs cur_pos board)



neighbors_idxs :: Pos -> Board -> [(Pos, Char)]
neighbors_idxs (i,j) board = map (\(p,m) -> (p, fromJust m)) $ 
                              filter (\a -> isJust (snd a)) tmp
 where tmp = 
        zip ([(i, j-1), (i-1, j), (i,j+1), (i+1,j)]) (neighbors (i,j) board)

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
