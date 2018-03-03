G52AFP Coursework 1 - Connect 4

James Simon Morris        | William Michael Hickling
psyjsmor@nottingham.ac.uk | psywmh@nottingham.ac.uk

----------------------------------------------------------------------

> import Data.Char
> import Data.List
> import System.IO

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 4
>
> depth :: Int
> depth = 6

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]
>
> type Row = [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X                               -- O < B < X
>               deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['1'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

----------------------------------------------------------------------

Next player to move is simply given by swapping between O and X:

> next :: Player -> Player
> next O = X
> next B = B
> next X = O

Empty Board defined by replicating the Blank Player:

> empty :: Board
> empty = replicate rows (replicate cols B)

End of game can be determined if the Board is full/filled with all
non-blank players:

> full :: Board -> Bool
> full = all (/= B) . concat

We must calculate whos turn it is by comparing the number of Xs and Os:

> turn :: Board -> Player
> turn g = if os <= xs then O else X
>          where
>             os = length (filter (== O) ps)
>             xs = length (filter (== X) ps)
>             ps = concat g

We use a function to determine win state for players:

> wins :: Player -> Board -> Bool
> wins p g = any line (rows ++ cols ++ dias)
>            where
>               line = all (== p) -- alter using 'win' variable
>               rows = g
>               cols = transpose g
>               dias = [diag g, diag (map reverse g)]
>
> diag :: Board -> [Player]
> diag g = [g !! n !! n | n <- [0..rows-1]]
>
> won :: Board -> Bool
> won g = wins O g || wins X g

The user(s) or computer can only select a cell that is 'valid', i.e. has
player value of B:

> valid :: Board -> Int -> Bool
> valid g i = 0 <= i && i < rows*cols && concat g !! i == B

Carry out move:

> move :: Board -> Int -> Player -> [Board]
> move g i p = if valid g i then [chop cols (xs ++ [p] ++ ys)]
>              else if i >= cols then move g (i-cols) p
>              else []
>              where
>                 (xs,B:ys) = splitAt i (concat g)
>
> chop :: Int -> [a] -> [[a]]
> chop n [] = []
> chop n xs = take n xs : chop n (drop n xs)

Get natural number from input and show prompt:

> getNat :: String -> IO Int
> getNat prompt = do putStr prompt
>                    xs <- getLine
>                    if xs /= [] && all isDigit xs && read xs > 0 && read xs <= cols then
>                        return (read xs)
>                    else
>                        do putStrLn "ERROR: Invalid number"
>                           getNat prompt
>
> prompt :: Player -> String
> prompt p = "Player " ++ show p ++ ", enter your move: "

Run game:

> connect4 :: IO ()
> connect4 =  run empty O

> run      :: Board -> Player -> IO()
> run g p  =  do cls
>                goto (1,1)
>                showBoard g
>                run' g p
>
> run'     :: Board -> Player -> IO()
> run' g p | wins O g  = putStrLn "Player O wins!\n"
>          | wins X g  = putStrLn "Player X wins!\n"
>          | full g    = putStrLn "It's a draw!\n"
>          | otherwise =
>               do i <- getNat (prompt p)
>                  case move g i p of
>                     [] -> do putStrLn "ERROR: Invalid move"
>                              run' g p
>                     [g'] -> run g' (next p)

Clear screen and move cursor position utilities:

> type Pos = (Int, Int)
>
> cls :: IO ()
> cls = putStr "\ESC[2J"
>
> goto :: Pos -> IO ()
> goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

Type to represent the Game tree that shall be used:

> data Tree a = Node a [Tree a]
>               deriving Show

Building the game tree using the player inputs

> gameTree :: Board -> Player -> Tree Board
> gameTree g p = Node g [gameTree g' (next p) | g' <- moves g p]
>
> moves :: Board -> Player -> [Board]
> moves g p
>    | won g       = []
>    | full g      = []
>    | otherwise   = concat [move g i p | i <- [0..((rows * cols)-1)]]

Pruning the Game tree to a specific depth

> prune :: Int -> Tree a -> Tree a
> prune 0 (Node x _) = Node x []
> prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

The minimax algorithm

> minimax :: Tree Board -> Tree (Board,Player)
> minimax (Node g [])
>    | wins O g  = Node (g,O) []
>    | wins X g  = Node (g,X) []
>    | otherwise = Node (g,B) []
> minimax (Node g ts)
>    | turn g == O = Node (g, minimum ps) ts'
>    | turn g == X = Node (g, maximum ps) ts'
>                    where
>                      ts' = map minimax ts
>                      ps  = [p | Node (_,p) _ <- ts']

A function that returns the best next move for the computer

> bestmove :: Board -> Player -> Board
> bestmove g p = head [g' | Node( g', p') _ <- ts, p' == best]
>                where
>                   tree = prune depth (gameTree g p)
>                   Node (_,best) ts = minimax tree

Human vs Computer, main function to run H vs C version

> main :: IO()
> main =  do hSetBuffering stdout NoBuffering
>            play empty O
>
> play     :: Board -> Player -> IO()
> play g p = do cls
>               goto (1,1)
>               putGrid g
>               play' g p
> play'    :: Board -> Player -> IO()
> play' g p
>     | wins O g = putStrLn "Player O wins!\n"
>     | wins X g = putStrln "Player X wins!\n"
>     | full g   = putStrln "It's a draw!\n"
>     | p == O   = do i <- getNat (prompt p)
>                     case move g i p of [] -> do putStrLn "ERROR: Invalid Move"
>                                                 play' g p
>                                        [g'] -> play g' (next p)
>     | p == X  = do putStr "Player X is thinking... "
>                    (play $! (bestmove g p))(next p)
