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

We must represent the grid in terms of Player's in order to determine
other conditions such as win or turn:

> type Grid = [[Player]]

Next player to move is simply given by swapping between O and X:

> next :: Player -> Player
> next O = X
> next B = B
> next X = O

Empty grid defined by replicating the Blank Player:

> empty :: Grid
> empty = replicate rows (replicate cols B)

End of game can be determined if the grid is full/filled with all
non-blank players:

> full :: Grid -> Bool
> full = all (/= B) . concat

We must calculate whos turn it is by comparing the number of Xs and Os:

> turn :: Grid -> Player
> turn g = if os <= xs then O else X
>          where
>             os = length (filter (== O) ps)
>             xs = length (filter (== X) ps)
>             ps = concat g

We use a function to determine win state for players:

> wins :: Player -> Grid -> Bool
> wins p g = any line (rows ++ cols ++ dias)
>            where
>               line = all (== p)
>               rows = g
>               cols = transpose g
>               dias = [diag g, diag (map reverse g)]
> 
> diag :: Grid -> [Player]
> diag g = [g !! n !! n | n <- [0..rows-1]]
> 
> won :: Grid -> Bool
> won g = wins O g || wins X g

The user(s) or computer can only select a cell that is 'valid', i.e. has
player value of B

> valid :: Grid -> Int -> Bool
> valid g i = 0 <= i && i < rows*cols && concat g !! i == B






> move :: Grid -> Int -> Player -> [Grid]
> move g i p = if valid g i then [chop cols (xs ++ [p] ++ ys)] else []
>              where
>                 (xs,B:ys) = splitAt i (concat g)

> chop :: Int -> [a] -> [[a]]
> chop n [] = []
> chop n xs = take n xs : chop n (drop n xs)

> getNat :: String -> IO Int
> getNat prompt = do putStr prompt
>                    xs <- getLine
>                    if xs /= [] && all isDigit xs then
>                        return (read xs)
>                    else
>                        do putStrLn "ERROR: Invalid number"
>                           getNat prompt

> connect4 :: IO ()
> connect4 = run empty O

> run :: Grid -> Player -> IO ()
> run g p = do cls
>              goto (1,1)
>              showBoard g
>              run' g p

> type Pos = (Int, Int)
> 
> cls :: IO ()
> cls = putStr "\ESC[2J"
> 
> goto :: Pos -> IO ()
> goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

> run' :: Grid -> Player -> IO ()
> run' g p | wins O g   = putStrLn "Player O wins!\n"
>          | wins X g   = putStrLn "Player X wins!\n"
>          | full g     = putStrLn "It's a draw!\n"
>          | otherwise =
>               do i <- getNat (prompt p)
>                  case move g ((i-1)`mod`cols) p of
>                     [] -> do putStrLn "ERROR: Invalid move"
>                              run' g p
>                     [g'] -> run g' (next p)

> prompt :: Player -> String
> prompt p = "Player " ++ show p ++ ", enter your move: "
