module Main where

import           Data.Maybe           (mapMaybe, fromMaybe)
import qualified Data.PQueue.Prio.Min as PQ
import           Data.Vector          (Vector, (!), (//))
import qualified Data.Vector          as V
import           System.Environment

type Board = Vector Int

data Direction = North | East | South | West

data Puzzle = Puzzle 
  { board :: Board
  , dist  :: Int
  , dim   :: Int 
  , blank :: Int
  , moves :: Int
  , from  :: Maybe Puzzle } deriving (Show, Eq, Ord)

data Key = Key Int deriving (Eq, Ord)

type Frontier = PQ.MinPQueue Key Puzzle

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
totalDist b n = sum [distance (b ! m2v n i j) n i j | i <- [0..n-1], j <- [0..n-1]]

mkPuzzle :: [Int] -> Puzzle
mkPuzzle xs = Puzzle b d n z 0 Nothing
  where
    b = V.fromList xs
    n = size b
    d = totalDist b n
    z = fromMaybe (error "Invalid board - no blank cell") (V.elemIndex 0 b)

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
neighbors p = mapMaybe (neighbor p) [North, East, South, West]

solve :: Frontier -> Puzzle
solve fr = if dist m == 0 then m else solve fr2
  where
    ((_, m), fr1) = PQ.deleteFindMin fr
    ms = case from m of
      Nothing -> xs
      Just n  -> filter (\x -> (board . snd $ x) /= board n) xs
    xs = zip ks ns
    ns = neighbors m
    ks = map Key [moves p + dist p | p <- ns]
    fr2 = foldr (uncurry PQ.insert) fr1 ms

main = do
  args <- getArgs
  let file = head args
  txt <- readFile file

  let s   = map words $ lines txt
      ns  = (map . map) read s :: [[ Int]]
      b   = concat . tail $ ns
      p   = mkPuzzle b
      sol = solve (PQ.fromList [(Key (dist p), p)])
      
  print sol


