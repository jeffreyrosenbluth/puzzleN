Solving and animating the 15 puzzle with haskell
================================================

We all have our favorite mini projects that we like
to implement when trying a new programming language.
One of mine is a solver for the 15 puzzle. In case your not familiar with
the 15 puzzle it\'s a classic game with 4 rows and 4 columns containing 15
tiles labed 1-15 and one blank space. The object is to order the tiles from
1 to 15 by repeatedly choosing a tile to slide into the blank space.
See the wikipedia article 15\-puzzle.

One reason I like to code up a
solver for the 15 puzzle is that the algorithm is a nice example of the
interplay between algorithms and data structures. In particular, we use the
A\* algorithm which relies on a prioriy queue.

Recently, I realized that: 

1. The only programming language I use (when given a choice) is haskell.  
2. For some reason I had never implemented a 15 puzzle solver in haskell. 
3. It would be nice to animiate the solution using the diagrams EDSL. 

This blog post is a literate haskell file so that you can
run the code if you choose. Lets get some imports out of the way.

> module Main where
>
> import           Data.Array.Unboxed
> import           Data.List                           (elemIndex)
> import           Data.List.Split                     (chunksOf)
> import           Data.Maybe                          (mapMaybe)
> import qualified Data.PQueue.Prio.Min                as PQ
> import           Diagrams.Prelude
> import           Diagrams.Backend.Rasterific.CmdLine
> import           System.Environment

Lets assume we already have a solution to a particular puzzle and write the
diagrams code to draw and animate it first. The solution takes the form 
`[Board]` where `Board` is an matrix of tiles. Each tile is a number between
1 and 15.

> type Board = UArray (Int, Int) Int

First we need to draw a `Board`, i.e convert it to a diagram.
Our strategy is to map a function that draws each tile onto the board then
concatenate the tile diagrams into a diagram of the puzzle board. Diagram\'s
has built in functions for vertically and hoerizontally concatenating lists
of diagrams so we convert the `Board` to a list.

> fromBoard :: Board -> [[Int]]
> fromBoard b = [row i | i <- [0..n]]
>   where
>     row i = [b ! (i, j) | j <- [0..n]]
>     n = snd . snd . bounds $ b

Assuming we have a function `drawTile` that makes an number into
a tile diagrams we can now create a diagram from a game board.

> boardDia :: Board -> Diagram B R2
> boardDia b = bg gray
>            . frame 0.1
>            . vcat' (with & sep .~ 0.075)
>            . map (hcat' (with & sep .~ 0.075))
>            . (map . map) drawTile $ fromBoard b 

And here is `drawTile`

> drawTile :: Int -> Diagram B R2
> drawTile 0 = square 1 # lw none
> drawTile s = text (show s) 
>            # fontSize (Local 1) 
>            # fc white
>            # scale 0.5
>            # bold
>           <> roundedRect 1 1 0.2 
>            # fc darkred

Now we need to assemble a bunch of board diagrams into a GIF.
All we need to do is pass a list of diagrams and delay times to `mainWith`, 
`[(Diagram B R2, Int)]`.

> dias :: [Board] -> [Diagram B R2]
> dias bs = map boardDia bs 

We show each board for 1 second and pause for three seconds before starting
looping.

> times :: Int -> [Int]
> times n = replicate (n-1) 100 ++ [300]

> gifs :: [Board] -> [(Diagram B R2, Int)]
> gifs bs = zip (dias bs) (times . length $ bs)

> fromString :: String -> [[Int]]
> fromString s = (map . map) read ws
>   where ws = map words (lines s)

Here is an example main program that solves a puzzle that is read in from
a text file. The format is that the first line contains a single integer
representing the dimension of the puzzle and each additional line is a string
of integers representing a row of the starting puzzle board.
Of course we still need to write, `solve`, `mkPuzzle`, and `boards`.

> main = do
>   putStrLn "Enter the name of the file containing the puzzle specification: "
>   txt <- readFile =<< getLine
>   let game = fromString txt
>       ([n], brd) = case game of
>         [] -> error "Invalid puzzle file"
>         x:xs -> (x, concat xs)
>   let p = solve . mkPuzzle n $ brd
>   mainWith $ gifs (boards p) 

We are going to search for a solution using the A\* algotihm.
We will keep track of the state of the game in an
algebraic data type called `Puzzle`.

The game state inludes the board, the number of moves up until this
point and a previous state (unless this it the start state). We
also cache the location of the blank
tile and the manhattan distance to the goal state; so that we only need
to calculate this things once.

Notice that the game state `Puzzle` recursively contains
the game state that preceeded it, except for the start state whose `previous`
field will contain `Nothing`. This will allow us recreate all of the intermediate
boards from the final solved board so that we can animate the game. This 
is what is done in the `boards` function.

> data Puzzle = Puzzle 
>   { board     :: Board
>   , dist      :: Int
>   , blank     :: (Int, Int)
>   , moves     :: Int
>   , previous  :: Maybe Puzzle 
>   } deriving (Show, Eq, Ord)

> dim :: Puzzle -> Int
> dim = (+1) . snd . snd . bounds . board

> boards :: Puzzle -> [Board]
> boards p = reverse $ brds p
>   where
>     brds q = case previous q of
>       Nothing -> [board q]
>       Just r  -> board q : brds r




 Territory to explore stored in a priority queue with priority
 equal to the moves made so far plus the manhattan distance.

 The possible moves.

> data Direction = North | East | South | West
>
> type Frontier = PQ.MinPQueue Int Puzzle

Manhattan distance of a tile with value v at position (i, j),
for a game of dimension n.

> manhattan :: Int -> Int -> Int -> Int  -> Int
> manhattan v n i j = if v == 0 then 0 else rowDist + colDist
>   where
>     rowDist = abs (i - ((v-1) `div` n))
>     colDist = abs (j - ((v-1) `mod` n))

 Manhattan distance of entire board.

> totalDist :: Board -> Int
> totalDist b = sum [manhattan (b ! (i, j)) n i j | i <- [0..n-1], j <- [0..n-1]]
>   where n = (+1) . snd . snd . bounds $ b

 Create a start state from a list of tiles.

> mkPuzzle :: Int -> [Int] -> Puzzle
> mkPuzzle n xs = Puzzle b d z 0 Nothing
>   where
>     b = listArray ((0, 0), (n-1, n-1)) xs
>     d = totalDist b
>     Just z' = elemIndex 0 xs
>     z = (z' `div` n, z' `mod` n)

 Update the game state after swithing the position of the blank
 and tile i j.

> update :: Puzzle -> Int -> Int -> Puzzle
> update p i j = p { board = b
>                  , dist = totalDist b
>                  , blank = (i, j)
>                  , moves = moves p + 1
>                  , previous = Just p }
>   where
>     b = b' // [(blank p, b' ! (i, j)), ((i, j), 0)]
>     b' = board p

 Find the the board that can be reached from the current state 
 by moving in the specified direction, being careful not to 
 move off the board.

> neighbor :: Puzzle -> Direction -> Maybe Puzzle
> neighbor p dir = case dir of
>   North -> if i <= 0   then Nothing else Just $ update p (i-1) j
>   East  -> if j >= n-1 then Nothing else Just $ update p i (j+1)
>   South -> if i >= n-1 then Nothing else Just $ update p (i+1) j
>   West  -> if j <= 0   then Nothing else Just $ update p i (j-1)
>   where
>     (i, j) = blank p
>     n = dim p

 All of the states that can be reached in one move from the
 current state.

> neighbors :: Puzzle -> [Puzzle]
> neighbors p = mapMaybe (neighbor p) [North, East, South, West]

> solve :: Puzzle -> Puzzle
> solve p = go (PQ.fromList [(dist p, p)])
>   where
>     go fr = if dist puzzle == 0 
>             then puzzle 
>             else go fr2
>       where
>         -- Retrieve the game state with the lowest priority and remove it from
>         -- the frontier.
>         ((_, puzzle), fr1) = PQ.deleteFindMin fr

>         -- If the new board is the smae as the previous board then
>         -- do not add it to the queue since it has already been explored.
>         ns = case previous puzzle of
>           Nothing -> neighbors puzzle
>           Just n  -> filter (\x -> board x /= board n) (neighbors puzzle)

>         -- The priority of a puzzle is the number of moves so far
>         -- plus the manhattan distance.
>         ps  = zip [moves q + dist q | q <- ns] ns
>         fr2 = foldr (uncurry PQ.insert) fr1 ps

