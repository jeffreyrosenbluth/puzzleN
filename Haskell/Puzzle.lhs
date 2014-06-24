We all have our favorite mini projects that we like
to implement when trying a new programming language.
One of mine is a solver for the 15-puzzle. In case your not familiar with
the 15-puzzle, it\'s a classic game with 4 rows and 4 columns containing 15
tiles labled 1-15 and one blank space. The object is to order the tiles from
1 to 15 by repeatedly choosing a tile to slide into the blank space.
See [15 puzzle](http://en.wikipedia.org/wiki/15_puzzlel).

![45 move puzzle](puzzle.gif)

One reason I like to code up a
solver for the 15-puzzle is that the algorithm is a nice example of the
interplay between algorithms and data structures. In particular, we use the
A\* algorithm which relies on a prioriy queue.

Recently, I realized that: 

- the only programming language I use (when given a choice) is haskell,
- for some reason I had never implemented a 15-puzzle solver in haskell, 
- it would be nice to animate the solution using the *diagrams* EDSL, 

and hence this blog post was born. We start by getting the imports out of the way.

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

Creating the animated GIF
-------------------------

Lets write the *diagrams* code to draw and animate a solution assuming we 
have already solved a puzzle. The solution takes the form 
`[Board]` where `Board` is an matrix of tiles. Each tile is a number between
1 and 15.

> type Board = UArray (Int, Int) Int

First we need to draw a single `Board`, i.e convert it to a diagram.
Our strategy is to map a function that draws each tile onto the board, then
concatenate the tile diagrams into a diagram of the puzzle board. *diagrams*
has built in functions for vertically and horizontally concatenating lists
of diagrams so we start by converting the `Board` to a list.

> fromBoard :: Board -> [[Int]]
> fromBoard b = [row i | i <- [1..n]]
>   where
>     row i = [b ! (i, j) | j <- [1..n]]
>     n = dim b

The dimension of the game is the upper bound of the array since we are using
1 as the starting indices for our array.

> dim :: Board -> Int
> dim = snd . snd . bounds 

Assuming we have a function `drawTile` that makes a number into
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
All we need to do is pass a list of diagrams and delay times 
`[(Diagram B R2, Int)]` to the `mainWith` function, choose a .gif file
extension when we run the program and *diagrams* will make an animated GIF.

> dias :: [Board] -> [Diagram B R2]
> dias bs = map boardDia bs 

We show each board for 1 second and pause for 3 seconds before repeating
the GIF loop.

> times :: Int -> [Int]
> times n = replicate (n-1) 100 ++ [300]

> gifs :: [Board] -> [(Diagram B R2, Int)]
> gifs bs = zip (dias bs) (times . length $ bs)

Here is an example main program that solves a puzzle read in from
a text file. The format of the puzzle file has as first line a single integer
representing the dimension of the puzzle and each additional line a string
of integers representing a row of the starting puzzle board. For example the
puzzle at the top of the post has file:

    4
     9  2  8  11  
     0  5 13   7
    15  1  4  10
     3 14  6  12
 
Of course we still need to write, `solve`, `mkGameState`, and `boards`.

> main = do
>   putStrLn "Enter the name of the file containing the puzzle specification: "
>   txt <- readFile =<< getLine
>   let game = fromString txt
>       ([n], brd) = case game of
>         [] -> error "Invalid puzzle file"
>         x:xs -> (x, concat xs)
>   let p = solve . mkGameState n $ brd
>   mainWith $ gifs (boards p) 

> fromString :: String -> [[Int]]
> fromString s = (map . map) read ws
>   where ws = map words (lines s)

The A\* algorithm
-----------------

We are going to search for a solution using the A\* algorithm.
We will keep track of the state of the game in an
algebraic data type called `GameState`.

The game state inludes the board, the number of moves up until this
point and a previous state (unless this it the start state). We
also cache the location of the blank
tile and the manhattan distance to the goal state; so that we only need
to calculate these things once.

Notice that `GameState` recursively contains
the game state that preceeded it (wrapped in a `Maybe`) , except for the start 
state whose `previous`
field will contain `Nothing`. This will allow us recreate all of the intermediate
boards from the final solved board so that we can animate the game. We use
the `boards` function to create the list containing each board from start to
finish.

> data GameState = GameState 
>   { board     :: Board
>   , dist      :: Int
>   , blank     :: (Int, Int)
>   , moves     :: Int
>   , previous  :: Maybe GameState 
>   } deriving (Show, Eq, Ord)


> boards :: GameState -> [Board]
> boards p = reverse $ brds p
>   where
>     brds q = case previous q of
>       Nothing -> [board q]
>       Just r  -> board q : brds r

The possible moves.

> data Direction = North | East | South | West

We create a priority queue `Frontier` whose priorities are the sum of the
moves made so far to reach the game state and the manhattan distance to
the goal state. This is a consistent heuristic function which guarantees
that the solution we find will take the minimum number of moves. The initial
`Frontier` contains only the start state. Then we recusively pop the minimum
game state from the queue and check to see if it is the goal, if it is we
are done, if not we calculate the states reachable by a legal game move 
(`neighbors`) and add them to the queue. Here\'s the code.

> type Frontier = PQ.MinPQueue Int GameState

Manhattan distance of a tile with value `v` at position `(i, j)`,
for a game of dimension `n`.

> manhattan :: Int -> Int -> Int -> Int  -> Int
> manhattan v n i j = if v == 0 then 0 else rowDist + colDist
>   where
>     rowDist = abs (i-1 - ((v-1) `div` n))
>     colDist = abs (j-1 - ((v-1) `mod` n))

Manhattan distance of entire board.

> totalDist :: Board -> Int
> totalDist b = sum [manhattan (b ! (i, j)) n i j | i <- [1..n], j <- [1..n]]
>   where n = dim b

Create a start state from a list of tiles.

> mkGameState :: Int -> [Int] -> GameState
> mkGameState n xs = GameState b d z 0 Nothing
>   where
>     b = listArray ((1, 1), (n, n)) xs
>     d = totalDist b
>     Just z' = elemIndex 0 xs
>     z = (1 + z' `div` n, 1 + z' `mod` n)

Update the game state after switching the position of the blank
and tile `(i, j)`.

> update :: GameState -> Int -> Int -> GameState
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

> neighbor :: GameState -> Direction -> Maybe GameState
> neighbor p dir = case dir of
>   North -> if i <= 1 then Nothing else Just $ update p (i-1) j
>   East  -> if j >= n then Nothing else Just $ update p i (j+1)
>   South -> if i >= n then Nothing else Just $ update p (i+1) j
>   West  -> if j <= 1 then Nothing else Just $ update p i (j-1)
>   where
>     (i, j) = blank p
>     n = dim . board $ p

All of the states that can be reached in one move from the
current state.

> neighbors :: GameState -> [GameState]
> neighbors p = mapMaybe (neighbor p) [North, East, South, West]

Finally, solve the puzzle.

> solve :: GameState -> GameState
> solve p = go (PQ.fromList [(dist p, p)])
>   where
>     go fr = if dist puzzle == 0 
>             then puzzle 
>             else go fr2
>       where
>         -- Retrieve the game state with the lowest priority
>         -- and remove it from the frontier.
>         ((_, puzzle), fr1) = PQ.deleteFindMin fr
>
>         -- If the new board is the smae as the previous board then
>         -- do not add it to the queue since it has already been
>         -- explored.
>         ns = case previous puzzle of
>           Nothing -> neighbors puzzle
>           Just n  -> filter (\x -> board x /= board n) 
>                             (neighbors puzzle)
>
>         -- The priority of a puzzle is the number of moves so far
>         -- plus the manhattan distance.
>         ps  = zip [moves q + dist q | q <- ns] ns
>         fr2 = foldr (uncurry PQ.insert) fr1 ps

You can find more puzzles in my github repo:
[puzzles](https://github.com/jeffreyrosenbluth/puzzleN/tree/master/puzzles).
Happy solving !
