% Copyright 2005 Brian Alliet

\documentclass[11pt]{article}
\usepackage{palatino}
\usepackage{fullpage}
\usepackage{parskip}
\usepackage{lhs}

\begin{document}

\title{Sudoku Solver}
\author{Brian Alliet}
\maketitle

\ignore{
\begin{code}
module Sudoku (
    Sudoku,
    makeSudoku, solve, eliminate, analyze, backtrack,
    main
    ) where

import Array
import Monad
import List (union,intersperse,transpose,(\\),nub,nubBy)
\end{code}
}

\section{Introduction}

This Haskell module implements a solver for Sudoku~\footnote{http://en.wikipedia.org/wiki/Sudoku} puzzles. It can solve
any Sudoku puzzle, even those that require backtracking.

\section{Data Types}

\begin{code}
data CellState a = Known a | Unknown [a] | Impossible deriving Eq
\end{code}

Each cell in a Sudoku grid can be in one of three states: ``Known'' if it has a known correct value~\footnote{Actually
this doesn't always means it is correct. While we are in the backtracking stage we make our guesses ``Known''.},
``Unknown'' if there is still more than one possible correct value, or ``Impossible'' if there is no value that can
possibly fit the cell. Sudoku grids with ``Impossible'' cells are quickly discarded by the {\tt solve} function.

\begin{code}
type Coords = (Int,Int)
type Grid a = Array Coords (CellState a)
newtype Sudoku a = Sudoku { unSudoku :: Grid a } deriving Eq
\end{code}

We represent a Sudoku grid as an Array indexed by integer coordinates. We additionally define a newtype wrapper for the
grid. The smart constructor, {\tt makeSudoku} verifies some invariants before creating the Sudoku value. All the public
API functions operate on the Sudoku type.

\begin{code}
instance Show a => Show (Sudoku a) where showsPrec p = showParen (p>0) . showsGrid . unSudoku
instance Show a => Show (CellState a) where showsPrec _ = showsCell
\end{code}

We define {\tt Show} instances for the above types.

\section{Internal Functions}

\begin{code}
size :: Grid a -> Int
size = (+1).fst.snd.bounds
\end{code}

{\tt size} returns the size (the width, height, and number of subboxes) for a Sudoku grid. We ensure Grid's are always
square and indexed starting at $(0,0)$ so simply incrementing either of the array's upper bounds is correct.

\begin{code}
getRow,getCol,getBox :: Grid a -> Int -> [(Coords,CellState a)]
getRow grid r = [let l = (r,c) in (l,grid!l)|c <- [0..size grid - 1]]
getCol grid c = [let l = (r,c) in (l,grid!l)|r <- [0..size grid - 1]]
getBox grid b = [let l = (r,c) in (l,grid!l)|r <- [boxR..boxR+boxN-1],c <- [boxC..boxC+boxN-1]]
    where
        boxN = intSqrt (size grid); boxR = b `quot` boxN * boxN; boxC = b `rem`  boxN * boxN

getBoxOf :: Grid a -> Coords -> [(Coords,CellState a)]
getBoxOf grid (r,c) = grid `getBox` ((r `quot` boxN * boxN) + (c `quot` boxN))
    where boxN = intSqrt (size grid)
\end{code}

{\tt getRow}, {\tt getCol}, and {\tt getBox} return the coordinates and values of the cell in row, column, or box
number {\tt n}, {\tt r}, or {\tt b}.

\begin{code}
getNeighbors :: Eq a => Grid a -> Coords -> [(Coords,CellState a)]
getNeighbors grid l@(r,c) = filter ((/=l).fst) 
                          $ foldr (union.($grid)) [] 
                          [(`getRow`r),(`getCol`c),(`getBoxOf`l)]
\end{code}

{\tt getNeighbors} returns the coordinates and values of all the neighbors of this cell.

\begin{code}
impossible :: Eq a => Grid a -> Coords -> [a]
impossible grid l = map snd $ justKnowns $ grid `getNeighbors` l
\end{code}

{\tt impossible} returns a list of impossible values for a given cell. The impossible values consist of the values any
``Known'' neighbors.

\begin{code}
justUnknowns :: [(Coords,CellState a)] -> [(Coords,[a])]
justUnknowns = foldr (\c -> case c of (p,Unknown xs) -> ((p,xs):); _ -> id) []

justKnowns :: [(Coords,CellState a)] -> [(Coords,a)]
justKnowns = foldr (\c -> case c of (p,Known x) -> ((p,x):); _ -> id) []
\end{code}

{\tt justUnknowns} and {\tt justKnowns} return only the Known or Unknown values (with the constructor stripped off)
from a list of cells.

\begin{code}
updateGrid :: Grid a -> [(Coords,CellState a)] -> Maybe (Grid a)
updateGrid _ [] = Nothing
updateGrid grid xs = Just $ grid // nubBy (\(x,_) (y,_) -> x==y) xs
\end{code}

{\tt updateGrid} applies a set of updates to a grid and returns the new grid only if it was updated.

\section{Public API}

\begin{code}
makeSudoku :: (Num a, Ord a, Enum a) => [[a]] -> Sudoku a
makeSudoku xs
    | not (all ((==size).length) xs) = error "error not a square"
    | (intSqrt size)^(2::Int) /= size = error "error dims aren't perfect squares"
    | any (\x -> x < 0 || x > fromIntegral size) (concat xs) = error "value out of range"
    | otherwise = Sudoku (listArray ((0,0),(size-1,size-1)) states)
    where
        size = length xs
        states = map f (concat xs)
        f 0 = Unknown [1..fromIntegral size]
        f x = Known x
\end{code}

{\tt makeSudoku} makes a {\tt Sudoku} value from a list of numbers. The given matrix must be square and have dimensions
that are a perfect square. The possible values for each cell range from 1 to the dimension of the square with ``0''
representing unknown values.\footnote{The rest of the code doesn't depend on any of this weird ``0'' is unknown
representation. In fact, it doesn't depend on numeric values at all. ``0'' is just used here because it makes
representing grids in Haskell source code easier.}

\begin{code}
eliminate :: Eq a => Sudoku a -> Maybe (Sudoku a)
eliminate (Sudoku grid) = fmap Sudoku $ updateGrid grid changes >>= sanitize
    where
        changes = concatMap findChange $ assocs grid
        findChange (l,Unknown xs) 
            = map ((,) l) 
            $ case filter (not.(`elem`impossible grid l)) xs of
                [] -> return Impossible
                [x] -> return $ Known x
                xs'
                    | xs' /= xs -> return $ Unknown xs'
                    | otherwise -> mzero
        findChange _ = mzero
        sanitize grid = return $ grid // [(l,Impossible) | 
            (l,x) <- justKnowns changes, x `elem` impossible grid l]
\end{code}

The {\tt eliminate} phase tries to remove possible choices for ``Unknowns'' based on ``Known'' values in the same row,
column, or box as the ``Unknown'' value. For each cell on the grid we find its ``neighbors'', that is, cells in the
same row, column, or box. Out of those neighbors we get a list of all the ``Known'' values. We can eliminate all of
these from our list of candidates for this cell. If we're lucky enough to eliminate all the candidates but one we have
a new ``Known'' value. If we're unlucky enough to have eliminates {\bf all} the possible candidates we have a new
``Impossible'' value.

After iterating though every cell we make one more pass looking for conflicting changes. {\tt sanitize} marks cells as
``Impossible'' if we have conflicting ``Known'' values.

\begin{code}
analyze :: Eq a => Sudoku a -> Maybe (Sudoku a)
analyze (Sudoku grid) = fmap Sudoku $ updateGrid grid $ nub [u |
            f <- map ($grid) [getRow,getCol,getBox],
            n <- [0..size grid - 1],
            u <- unique (f n)]
    where
        unique xs = foldr f [] $ foldr (union.snd) [] unknowns \\ map snd (justKnowns xs)
            where
                unknowns = justUnknowns xs
                f c = case filter ((c`elem`).snd) unknowns of
                    [(p,_)] -> ((p,Known c):)
                    _ -> id
\end{code}

The {\tt analyze} phase tries to turn ``Unknowns'' into ``Knowns'' when a certain ``Unknown'' is the only cell that
contains a value needed in a given row, column, or box. We apply each of the functions {\tt getRow}, {\tt getCol}, and
{\tt getBox} to all the indices on the grid, apply {\tt unique} to each group, and update the array with the
results. {\tt unique}  gets a list of all  the unknown cells in the group and finds all the unknown values in each of
those cells. Each of these values are iterated though looking for a value that is only contained in one cell. If such a
value is found the cell containing it must be that value.

\begin{code}
backtrack :: (MonadPlus m, Eq a) => Sudoku a -> m (Sudoku a)
backtrack (Sudoku grid) = case (justUnknowns (assocs grid)) of
    [] -> return $ Sudoku grid
    ((p,xs):_) -> msum $ map (\x -> solve $ Sudoku $ grid // [(p,Known x)]) xs
\end{code}

Sometimes the above two phases still aren't enough to solve a puzzle. For these rare puzzles backtracking is required.
We attempt to solve the puzzle by replacing the first ``Unknown'' value with each of the candidate values and solving
the resulting puzzles. Hopefully at least one of our choices will result in a solvable puzzle.

We could actually solve any puzzle using backtracking alone, although this would be very inefficient. The above
functions simplify most puzzles enough that the backtracking phase has to do hardly any work.

\begin{code}
solve :: (MonadPlus m, Eq a) => Sudoku a -> m (Sudoku a)
solve sudoku = 
    case eliminate sudoku of
        Just new 
            | any (==Impossible) (elems (unSudoku new))-> mzero
            | otherwise -> solve new
        Nothing -> case analyze sudoku of
            Just new -> solve new
            Nothing -> backtrack sudoku
\end{code}

{\tt solve} glues all the above phases together. First we run the {\tt eliminate} phase. If that found the puzzle  to
be unsolvable we abort immediately. If {\tt eliminate} changed the grid we go though the {\tt eliminate} phase again
hoping to eliminate more. Once {\tt eliminate} can do no more work we move on to the {\tt analyze} phase. If this
succeeds in doing some work we start over again with the {\tt eliminate} phase. Once {\tt analyze} can do no more work
we have no choice but to resort to backtracking. (However in most cases backtracking won't actually do anything because
the puzzle is already solved.)

\begin{code}
showsCell :: Show a => CellState a -> ShowS
showsCell (Known x) = shows x
showsCell (Impossible) = showChar 'X'
showsCell (Unknown xs) = \rest -> ('(':) 
                       $ foldr id (')':rest)
                       $ intersperse (showChar ' ')
                       $ map shows xs
\end{code}

{\tt showCell} shows a cell.

\begin{code}
showsGrid :: Show a => Grid a -> ShowS
showsGrid grid = showsTable [[grid!(r,c) | c <- [0..size grid-1]] | r <- [0..size grid-1]]
\end{code}

{\tt showGrid} show a grid.

\begin{code}
-- FEATURE: This is pretty inefficient
showsTable :: Show a => [[a]] -> ShowS
showsTable xs = (showChar '\n' .) $ showString $ unlines $ map (concat . intersperse " ") xs''
    where
        xs' = (map.map) show xs
        colWidths = map (max 2 . maximum . map length) (transpose xs')
        xs'' = map (zipWith (\n s -> s ++ (replicate (n - length s) ' ')) colWidths) xs'
\end{code}

{\tt showsTable} shows a table (or matrix). Every column has the same width so things line up.

\begin{code}
intSqrt :: Integral a => a -> a
intSqrt n
    | n < 0 = error "intSqrt: negative n"
    | otherwise = f n
    where
        f x = if y < x then f y else x
            where y = (x + (n `quot` x)) `quot` 2
\end{code}

{\tt intSqrt} is Newton`s Iteration for finding integral square roots.

\ignore{
\begin{code}
test :: Sudoku Int
test = makeSudoku [
    [0,6,0,1,0,4,0,5,0],
    [0,0,8,3,0,5,6,0,0],
    [2,0,0,0,0,0,0,0,1],
    [8,0,0,4,0,7,0,0,6],
    [0,0,6,0,0,0,3,0,0],
    [7,0,0,9,0,1,0,0,4],
    [5,0,0,0,0,0,0,0,2],
    [0,0,7,2,0,6,9,0,0],
    [0,4,0,5,0,8,0,7,0]]

test2 :: Sudoku Int
test2 = makeSudoku [
    [0,7,0,0,0,0,8,0,0],
    [0,0,0,2,0,4,0,0,0],
    [0,0,6,0,0,0,0,3,0],
    [0,0,0,5,0,0,0,0,6],
    [9,0,8,0,0,2,0,4,0],
    [0,5,0,0,3,0,9,0,0],
    [0,0,2,0,8,0,0,6,0],
    [0,6,0,9,0,0,7,0,1],
    [4,0,0,0,0,3,0,0,0]]

testSmall :: Sudoku Int
testSmall = makeSudoku [
    [1,0,0,0,0,0,0,0,0],
    [0,0,2,7,4,0,0,0,0],
    [0,0,0,5,0,0,0,0,4],
    [0,3,0,0,0,0,0,0,0],
    [7,5,0,0,0,0,0,0,0],
    [0,0,0,0,0,9,6,0,0],
    [0,4,0,0,0,6,0,0,0],
    [0,0,0,0,0,0,0,7,1],
    [0,0,0,0,0,1,0,3,0]]

testHard :: Sudoku Int
testHard = makeSudoku [
    [0,0,0,8,0,2,0,0,0],
    [5,0,0,0,0,0,0,0,1],
    [0,0,6,0,5,0,3,0,0],
    [0,0,9,0,1,0,8,0,0],
    [1,0,0,0,0,0,0,0,2],
    [0,0,0,9,0,7,0,0,0],
    [0,6,1,0,3,0,7,8,0],
    [0,5,0,0,0,0,0,4,0],
    [0,7,2,0,4,0,1,5,0]]

testHard2 :: Sudoku Int
testHard2 = makeSudoku [
    [3,0,0,2,0,0,9,0,0],
    [0,0,0,0,0,0,0,0,5],
    [0,7,0,1,0,4,0,0,0],
    [0,0,9,0,0,0,8,0,0],
    [5,0,0,0,7,0,0,0,6],
    [0,0,1,0,0,0,2,0,0],
    [0,0,0,3,0,9,0,4,0],
    [8,0,0,0,0,0,0,0,0],
    [0,0,6,0,0,5,0,0,7]]

testHW :: Sudoku Int
testHW = makeSudoku [
    [0,0,0,1,0,0,7,0,2],    
    [0,3,0,9,5,0,0,0,0],
    [0,0,1,0,0,2,0,0,3],
    [5,9,0,0,0,0,3,0,1],
    [0,2,0,0,0,0,0,7,0],
    [7,0,3,0,0,0,0,9,8],
    [8,0,0,2,0,0,1,0,0],
    [0,0,0,0,8,5,0,6,0],
    [6,0,5,0,0,9,0,0,0]]

testTough :: Sudoku Int
testTough = makeSudoku $ map (map read . words) $ lines $
 "8 3 0  0 0 0  0 4 6\n"++
 "0 2 0  1 0 4  0 3 0\n"++
 "0 0 0  0 0 0  0 0 0\n"++
 "0 0 2  9 0 6  5 0 0\n"++
 "1 4 0  0 0 0  0 2 3\n"++
 "0 0 5  4 0 3  1 0 0\n"++
 "0 0 0  0 0 0  0 0 0\n"++
 "0 6 0  3 0 8  0 7 0\n"++
 "9 5 0  0 0 0  0 6 2\n"

testDiabolical :: Sudoku Int 
testDiabolical = makeSudoku $ map (map read . words) $ lines $
  "8 0 0  7 0 1  0 0 2\n"++
  "0 0 6  0 0 0  7 0 0\n"++
  "0 1 7  0 0 0  8 9 0\n"++
  "0 0 0  1 7 3  0 0 0\n"++
  "7 0 0  0 0 0  0 0 6\n"++
  "0 0 0  9 5 6  0 0 0\n"++
  "0 9 5  0 0 0  4 1 0\n"++
  "0 0 8  0 0 0  5 0 0\n"++
  "3 0 0  6 0 5  0 0 7\n"

main :: IO ()
main = do
    let
        solve' p = case solve p of
            [] -> fail $ "couldn't solve: " ++ show p
            sols -> return sols
    mapM_ (\p -> solve' p >>= putStrLn.show) [test,test2,testSmall,testHard,testHard2,testHW,testTough,testDiabolical]
    return ()

\end{code}
}

\end{document}
