module Main where

import qualified Data.Vector as V
import           Data.Vector (Vector, (!), (//))
import           Data.Maybe  (catMaybes)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set    as S
import           Data.Set    (Set, deleteFindMin)
import           System.Environment

type Board = Vector Int

data Direction = North | East | South | West

data Puzzle = Puzzle 
  { board :: Board
  , dist  :: Int
  , dim   :: Int 
  , blank :: Int
  , moves :: Int
  , from  :: Maybe Puzzle } deriving Show

instance Eq Puzzle where
  p == q = board p == board q

-- Defining g <= h as g' <= h' is faster but breaking ties as below
-- can yield more optiaml solutions.
instance Ord Puzzle where
  g <= h
    | g' < h' = True
    | g' > h' = False
    | otherwise = mg <= mh
    where
      g' = mg + dist g
      h' = mh + dist h
      mg = moves g
      mh = moves h
  
type Frontier = Set Puzzle

-- Convert martrix indices to array index.
m2v :: Int -> Int -> Int -> Int
m2v n row col = n * row + col

-- Convert array index to matrix indices.
v2m :: Int -> Int -> (Int, Int)
v2m n i = (i `div` n, i `mod` n)

-- Get the dimension of a board.
size :: Board -> Int
size b = round .sqrt . fromIntegral . V.length $ b

-- Manhattan distance of board b at cell (i, j).
distance :: Int -> Int -> Int -> Int  -> Int
distance v n i j = if v == 0 then 0 else rdist + cdist
  where
    rdist = abs (i - ((v-1) `div` n))
    cdist = abs (j - ((v-1) `mod` n))

-- Manhattan distance of entire board.
totalDist :: Board -> Int -> Int
totalDist b n = sum [distance (b ! (m2v n i j)) n i j | i <- [0..n-1], j <- [0..n-1]]

mkPuzzle :: [Int] -> Puzzle
mkPuzzle xs = Puzzle b d n z 0 Nothing
  where
    b = V.fromList xs
    n = size b
    d = totalDist b n
    z = case V.elemIndex 0 b of
          Just x  -> x
          Nothing -> error "Invalid board - no blank cell"

update :: Puzzle -> Int -> Int -> Puzzle
update p i j = p { board = b
                 , dist = totalDist b n
                 , blank = k
                 , moves = moves p + 1
                 , from = Just p }
  where
    k = m2v n i j
    b = b' // [(blank p, b' ! k), (k, 0)]
    b' = board p
    n = dim p

neighbor :: Puzzle -> Direction -> Maybe Puzzle
neighbor p dir = case dir of
  North -> if i <= 0   then Nothing else Just $ update p (i-1) j
  East  -> if j >= n-1 then Nothing else Just $ update p i (j+1)
  South -> if i >= n-1 then Nothing else Just $ update p (i+1) j
  West  -> if j <= 0   then Nothing else Just $ update p i (j-1)
  where
    (i, j) = v2m n (blank p)
    n = dim p

neighbors :: Puzzle -> [Puzzle]
neighbors p = catMaybes . map (neighbor p) $ [North, East, South, West]

solve :: Frontier -> Puzzle
solve fr = if dist m == 0 then m else solve fr2
  where
    (m, fr1) = deleteFindMin fr
    ms = case from m of
      Nothing -> ns
      Just n  -> filter (\x -> board x /= board n) ns
    ns = neighbors m
    fr2 = foldr S.insert fr1 ms

p45 = mkPuzzle [9,2,8,11,0,5,13,7,15,1,4,10,3,14,6,12]
p09 = mkPuzzle [2,0,3,4,1,10,6,8,5,9,7,12,13,14,11,15]
p32 = mkPuzzle [3,1,6,4,5,0,9,7,10,2,11,8,13,15,14,12]
p27 = mkPuzzle [5,8,7,1,4,6,3,0,2]
p31 = mkPuzzle [8,6,7,2,5,4,3,0,1]

main = do
  args <- getArgs
  let file = head args
  txt <- readFile file
  let s = map words $ lines txt
      ns = ((map . map) read s) ::[[ Int]]
      b = concat . tail $ ns
      sol = solve (S.fromList [mkPuzzle $  b])
  putStrLn $ show sol


