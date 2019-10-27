+++
title = "Talk:Solve a Holy Knight's tour"
description = ""
date = 2015-01-30T00:50:25Z
aliases = []
[extra]
id = 17667
[taxonomies]
categories = []
tags = []
+++

==Needs a better description of the task and method of solving it.==
If it is like a Hidato puzzle then maybe use a description of such a puzzle as a basis for putting together a description of this tour. We could also do with an algorithm for solving it as well as being pointed at code if possible. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:03, 1 June 2014 (UTC)

It would also be nice to know if the solution must be a closed Knight's tour or not.
--[[User:SteveWampler|SteveWampler]] ([[User talk:SteveWampler|talk]]) 22:53, 1 June 2014 (UTC)
: There is no requirement for the tour to be closed. The REXX solution is not closed, but neither is the board exactly as in the example. The task asks that the example is solved, there is more than 1 solution, any is acceptable. You may show other boards in addition with discretion.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 14:51, 2 June 2014 (UTC)

:: The REXX solution (the board) has been fixed, the placement of the pennies was in error;   the board for the REXX language now matches the task's requirement. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:50, 30 January 2015 (UTC) 

: So are the circles in the example the pennies?  If so, shouldn't the solutions be trying to visit all the *rest* of the squares?  These all seem to be trying specifically to visit the circles.. --[[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 19:46, 29 January 2015 (UTC)

: The numbers are the square without pennies. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:53, 29 January 2015 (UTC)


### Haskell Entry

A note mostly for [[User:Cromachina|Cromachina]]: currently the Haskell entry is nice but slow. Perhaps it's worth adding a second alternative version that is very similar but uses one STUArray (mutable arrays of ints to ints) to keep the game table, as in D entry, and compare the performances.

This is a start point for the second Haskell version, I have optimized it in some simple ways and it's almost three times faster, but it still uses an immutable array:


```haskell

    import qualified Data.Array.Unboxed as Arr
    import Data.List (transpose, intercalate)
    import Data.Maybe (listToMaybe, mapMaybe)
    import Data.Int (Int8)

    type Cell = Int8 -- This can be Int if KnightBoard is mutable.
    type Position = (Int, Int)
    type KnightBoard = Arr.UArray Position Cell

    notUsable = -1 :: Cell
    emptyCell = 0 :: Cell

    toCell :: Char -> Cell
    toCell '0' = emptyCell
    toCell '1' = 1
    toCell _   = notUsable

    countUsable :: KnightBoard -> Cell
    countUsable board = fromIntegral $ length $ filter (/= notUsable) (Arr.elems board)

    toBoard :: [String] -> (KnightBoard, Cell)
    toBoard strs = (board, countUsable board)
        where
            height = length strs
            width  = minimum $ map length strs
            board  = Arr.listArray ((0, 0), (width - 1, height - 1))
                     . map toCell . concat . transpose $ map (take width) strs

    showCell :: Cell -> String
    showCell (-1) = "  ." -- notUsable
    showCell n    = replicate (3 - length nn) ' ' ++ nn
        where nn = show n

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : (chunksOf n $ drop n xs)

    showBoard :: KnightBoard -> String
    showBoard board = intercalate "\n" . map concat . transpose
                      . chunksOf (height + 1) . map showCell $ Arr.elems board
        where (_, (_, height)) = Arr.bounds board

    add :: Num a => (a, a) -> (a, a) -> (a, a)
    add (a, b) (x, y) = (a + x, b + y)

    within :: Ord a => ((a, a), (a, a)) -> (a, a) -> Bool
    within ((a, b), (c, d)) (x, y) = a <= x && x <= c && b <= y && y <= d

    -- Enumerate valid moves given a board and a knight's position.
    validMoves :: KnightBoard -> Position -> [Position]
    validMoves board position = filter isValid plausible
        where
            bound       = Arr.bounds board
            plausible   = map (add position) [(1, 2), (2, 1), (2, -1), (-1, 2),
                                              (-2, 1), (1, -2), (-1, -2), (-2, -1)]
            isValid pos = within bound pos && (board Arr.! pos) == emptyCell

    -- Solve the knight's tour with a simple Depth First Search.
    solveKnightTour :: (KnightBoard, Cell) -> Maybe KnightBoard
    solveKnightTour (board, nUsable) = solve board 1 initPosition
        where
            initPosition = fst $ head $ filter ((== 1) . snd) $ Arr.assocs board
            solve :: KnightBoard -> Cell -> Position -> Maybe KnightBoard
            solve boardA depth position =
                if depth == nUsable
                    then Just boardB
                    else listToMaybe $ mapMaybe (solve boardB $ depth + 1) $ validMoves boardB position
                where
                    boardB = boardA Arr.// [(position, depth)]

    tourExA :: [String]
    tourExA =  [" 000    "
               ," 0 00   "
               ," 0000000"
               ,"000  0 0"
               ,"0 0  000"
               ,"1000000 "
               ,"  00 0  "
               ,"   000  "]

    tourExB :: [String]
    tourExB = ["-----1-0-----"
              ,"-----0-0-----"
              ,"----00000----"
              ,"-----000-----"
              ,"--0--0-0--0--"
              ,"00000---00000"
              ,"--00-----00--"
              ,"00000---00000"
              ,"--0--0-0--0--"
              ,"-----000-----"
              ,"----00000----"
              ,"-----0-0-----"
              ,"-----0-0-----"]

    main = flip mapM_ [tourExA, tourExB]
        (\board -> case solveKnightTour $ toBoard board of
            Nothing       -> putStrLn "No solution.\n"
            Just solution -> putStrLn $ showBoard solution ++ "\n")

```

