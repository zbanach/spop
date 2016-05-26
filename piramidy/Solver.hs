{-|
Module      : Solver
Description : Algorytm rozwiązywania łamigłówki "Piramidy"
Copyright   : Zbigniew Banach, Michał Breiter 2016

Moduł udostęnia funkcję rozwiązującą łamigłówkę "Piramidy".
-}
module Solver (solve, Solution(Solution,NoSolution)) where

import Prelude
import Data.List
import Data.Maybe

import Pyramids


-- | Typ reprezentujący rozwiązanie łamigłówki (lub jego brak).
data Solution = NoSolution | Solution Board

-- | Funkcja rozwiązująca łamigłówkę "Piramidy" dla podanego zestawu krawędzi planszy.
-- Zwraca planszę spełniającą warunki łamigłówki lub NoSolution, jeżeli rozwiązanie nie istnieje.
solve :: Edges -> Solution
solve edges = solve' (emptyBoard edges) (0, 0)

solve' :: Board -> Coordinates -> Solution
solve' board (row, col) = tryPyramid (sizeOf board) board (row, col)

tryPyramid :: Int -> Board -> Coordinates -> Solution
tryPyramid 0 _     _           = NoSolution
tryPyramid i board coordinates = 
    if canBePlaced i board coordinates && isValid nextSolution then
        nextSolution
    else
        nextPyramid
    where
        nextSolution = nextCell newBoard coordinates
        newBoard = place board coordinates i
        nextPyramid = tryPyramid (i - 1) board coordinates
        isValid NoSolution = False
        isValid _          = True
        
nextCell board (row, col)
    | col == n - 1 && row == n - 1 = Solution board
    | col == n - 1                 = solve' board (row + 1, 0)
    | otherwise                    = solve' board (row, col + 1)
    where
        n = sizeOf board

-- | Funkcja sprawdza, czy piramida o wysokości `pyramid` może być ustawiona na polu planszy 
-- o podanych współrzędnych. Zwraca True jeśli po umieszczeniu piramidy na polu spełnione 
-- będą warunki łamigłówki, tzn.:
--  1) w tym samym rzędzie i tej samej kolumnie nie ma już piramidy o takiej samej wysokości,
--  2) widoczna jest odpowiednia liczba piramid, określona przez liczby na krawędziach planszy.
canBePlaced pyramid board coordinates = isOnlyOneInRowAndColumn board coordinates pyramid
    && fulfillsPyramidCondition board coordinates pyramid

-- | Zwraca True jeśli w danym rzędzie i kolumnie nie ma piramidy o wysokości `pyramid`.
isOnlyOneInRowAndColumn board (row, col) pyramid = 
    not $ or $ map (\i -> (getElem board (i, col) == pyramid) 
        || (getElem board (row, i) == pyramid)) [0..(sizeOf board) - 1]

-- | Zwraca True jeśli po postawieniu piramidy o wysokości `pyramid` na polu o podanych współrzędnych
-- spełniony będzie warunek widoczności, tzn. w danej kolumnie od góry i dołu, a w rzędzie od lewej
-- i prawej strony widoczna będzie liczba piramid określona na odpowiednich krawędziach planszy.
fulfillsPyramidCondition board (row, col) pyramid =
    visibleFromTop && visibleFromBottom && visibleFromLeft && visibleFromRight
    where
        visibleFromTop    = visible (top !! col)    (getCol board col)
        visibleFromBottom = visible (bottom !! col) (reverse $ getCol board col)
        visibleFromLeft   = visible (left !! row)   (getRow board row)
        visibleFromRight  = visible (right !! row)  (reverse $ getRow board row)
        (Board edges _)   = board
        (Edges top bottom left right) = edges

-- | Zwraca True jeśli dla piramid na podanej liście spełniony jest warunek widoczności
-- (od lewej strony listy musi być widoczne tyle piramid, ile jest podane w pierwszym argumencie).
-- Jeśli lista zawiera puste pola (piramidy o wysokości 0), to liczba widocznych piramid jest 
-- szacowana w następujący sposób:
--  1) minimalna liczba widocznych piramid (minVisible) to liczba piramid widocznych na liście
--     otrzymanej przez uzupełnienie pustych pól pozostałymi piramidami w malejącej kolejności
--  2) maksymalna liczba widocznych piramid (maxVisible) to liczba piramid widocznych na liście
--     otrzymanej przez uzupełnienie pustych pól pozostalymi piramidami w rosnącej kolejności
-- Warunek widoczności jest spełniony, jeśli oczekiwana liczba widocznych piramid określona na
-- krawędzi planszy mieści się w przedziale [minVisible; maxVisible].
visible Nothing         pyramids = True
visible (Just expected) pyramids = expected >= minVisible && expected <= maxVisible
    where
        minVisible = countVisiblePyramids $ fillEmptyFieldsWithRemainingPyramids pyramids False
        maxVisible = countVisiblePyramids $ fillEmptyFieldsWithRemainingPyramids pyramids True

-- | Wypełnia puste pola na liście brakującymi piramidami w rosnącej (gdy ascendingOrder == True) 
-- lub malejącej (ascendingOrder == False) kolejności.
-- Np. wynikiem dla listy piramid [2,5,0,0,0] i ascendingOrder == False będzie [2,5,4,3,1].
fillEmptyFieldsWithRemainingPyramids fields ascendingOrder = replaceIfZero fields remaining
    where
        replaceIfZero []     []     = []
        replaceIfZero (0:xs) (y:ys) = y:replaceIfZero xs ys
        replaceIfZero (x:xs) y      = x:replaceIfZero xs y
        remaining = [ x | x <- range, not $ x `elem` fields ]
        range = if ascendingOrder then [1..n] else [n, n - 1..1]
        n = length fields

-- | Zwraca liczbę piramid widocznych na podanej liście (z perspektywy początku listy).
-- Piramida jest widoczna, jeśli jest wyższa od każdej poprzedzającej ją piramidy.
countVisiblePyramids lst = 
    countVisiblePyramids' lst 0 
    where
        countVisiblePyramids' []     _   = 0
        countVisiblePyramids' (x:xs) max
            | x > max   = 1 + countVisiblePyramids' xs x
            | otherwise = countVisiblePyramids' xs max

instance Show Solution where
    show NoSolution = "Rozwiązanie nie istnieje"
    show (Solution board) = show board

