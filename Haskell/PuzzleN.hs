-----------------------------------------------------------------------------
-- Program     :  puzzleN
-- Copyright   :  (c) 2014 Jeffrey Rosenbluth
--
-- Solve the generalized 8 puzzle using the A* aglorithm
-- with manhattan distance heuristic.
--
-----------------------------------------------------------------------------

module Main where

import           Data.List.Split                     (chunksOf)
import           Data.Maybe                          (mapMaybe, fromMaybe)
import qualified Data.PQueue.Prio.Min                as PQ
import           Data.Vector                         (Vector, (!), (//))
import qualified Data.Vector                         as V
import           Diagrams.Prelude
import           Diagrams.Backend.Rasterific.CmdLine
import           System.Environment

-- | The puzzle board is represented as a 1 dimensional vector of size
--   dim^2.
type Board = Vector Int

-- | The possible moves.
data Direction = North | East | South | West

-- | The game state inludes the board, the number of moves up until this
--   point and a previous state (unless this it the start state). We
--   also cache the dimension of the board, the location of the blank
--   tile and the manhattan distance to the goal state.
data Puzzle = Puzzle 
  { board     :: Board
  , dist      :: Int
  , dim       :: Int 
  , blank     :: Int
  , moves     :: Int
  , previous  :: Maybe Puzzle 
  } deriving (Show, Eq, Ord)

-- | Territory to explore stored in a priority queue with priority
--   equal to the moves made so far plus the manhattan distance.
type Frontier = PQ.MinPQueue Int Puzzle

-- Convert martrix indices to array index.
m2v :: Int -> Int -> Int -> Int
m2v n row col = n * row + col

-- Convert array index to matrix indices.
v2m :: Int -> Int -> (Int, Int)
v2m n i = (i `div` n, i `mod` n)

-- Get the dimension of a board.
size :: Board -> Int
size b = round . sqrt . fromIntegral . V.length $ b

-- | Manhattan distance of a tile with value v at position (i, j),
--   for a game of dimension n.
manhattan :: Int -> Int -> Int -> Int  -> Int
manhattan v n i j = if v == 0 then 0 else rowDist + colDist
  where
    rowDist = abs (i - ((v-1) `div` n))
    colDist = abs (j - ((v-1) `mod` n))

-- | Manhattan distance of entire board.
totalDist :: Board -> Int -> Int
totalDist b n = sum [manhattan (b ! m2v n i j) n i j | i <- [0..n-1], j <- [0..n-1]]

-- | Create a start state from a list of tiles.
mkPuzzle :: [Int] -> Puzzle
mkPuzzle xs = Puzzle b d n z 0 Nothing
  where
    b = V.fromList xs
    n = size b
    d = totalDist b n
    z = fromMaybe (error "Invalid board - no blank cell") (V.elemIndex 0 b)

-- | Update the game state after swithing the position of the blank
--   and tile i j.
update :: Puzzle -> Int -> Int -> Puzzle
update p i j = p { board = b
                 , dist = totalDist b n
                 , blank = k
                 , moves = moves p + 1
                 , previous = Just p }
  where
    k = m2v n i j
    b = b' // [(blank p, b' ! k), (k, 0)]
    b' = board p
    n = dim p

-- | Find the the board that can be reached from the current state 
--   by moving in the specified direction, being careful not to 
--   move off the board.
neighbor :: Puzzle -> Direction -> Maybe Puzzle
neighbor p dir = case dir of
  North -> if i <= 0   then Nothing else Just $ update p (i-1) j
  East  -> if j >= n-1 then Nothing else Just $ update p i (j+1)
  South -> if i >= n-1 then Nothing else Just $ update p (i+1) j
  West  -> if j <= 0   then Nothing else Just $ update p i (j-1)
  where
    (i, j) = v2m n (blank p)
    n = dim p

-- | All of the states that can be reached in one move from the
--   current state.
neighbors :: Puzzle -> [Puzzle]
neighbors p = mapMaybe (neighbor p) [North, East, South, West]

solve :: Puzzle -> Puzzle
solve p = go (PQ.fromList [(dist p, p)])
  where
    go fr = if dist puzzle == 0 
            then puzzle 
            else go fr2
      where
        -- Retrieve the game state with the lowest priority and remove it from
        -- the frontier.
        ((_, puzzle), fr1) = PQ.deleteFindMin fr

        -- If the new board is the smae as the previous board then
        -- do not add it to the queue since it has already been explored.
        ns = case previous puzzle of
          Nothing -> neighbors puzzle
          Just n  -> filter (\x -> board x /= board n) (neighbors puzzle)

        -- The priority of a puzzle is the number of moves so far
        -- plus the manhattan distance.
        ps  = zip [moves q + dist q | q <- ns] ns
        fr2 = foldr (uncurry PQ.insert) fr1 ps

boards :: Puzzle -> [[Int]]
boards p = map V.toList (reverse $ brds p)
  where
    brds q = case previous q of
      Nothing -> [board q]
      Just r  -> board q : brds r

boardDia :: Int -> [Int] -> Diagram B R2
boardDia n ns = bg lightgray
              . frame 0.1 
              . vcat' (with & sep .~ 0.05) 
              . map (hcat' (with & sep .~ 0.05))
              . (map . map) draw $ rows
  where
    rows = chunksOf n ns

draw :: Int -> Diagram B R2
draw s = text (label s) 
       # fontSize (Normalized 0.12) 
       # bold
      <> roundedRect 1 1 0.2 
       # fc darkseagreen
       # lw thick
  where
    label 0 = ""
    label n = show n

dias :: Int -> [[Int]] -> [Diagram B R2]
dias n ns = map (boardDia n) ns 

times :: Int -> [Int]
times n = replicate (n-1) 100 ++ [300]

gifs :: Int -> [[Int]] -> [(Diagram B R2, Int)]
gifs n ns = zip (dias n ns) (times . length $ ns)

fromString :: String -> [[Int]]
fromString s = (map . map) read ws
  where ws = map words (lines s)

main = do
  putStrLn "Enter the name of the file containing the puzzle specification: "
  txt <- readFile =<< getLine
  let game = fromString txt
      ([n], brd) = case game of
        [] -> error "Invalid puzzle file"
        x:xs -> (x, concat xs)
  let p = solve . mkPuzzle $ brd
  mainWith $ gifs n (boards p) 


