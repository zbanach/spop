{-|
Module      : Pyramids
Description : Model funkcyjny łamigłówki "Piramidy"
Copyright   : Zbigniew Banach, Michał Breiter 2016

Moduł zawiera definicje typów danych wykorzystywanych przy rozwiązywaniu łamigłówki "Piramidy" 
oraz funkcyjny model planszy.
-}
module Pyramids where

import Prelude
import Data.List


-- | Typ reprezentujący zestaw danych wejściowych łamigłówki, czyli zbiór czterech krawędzi planszy
-- (w kolejności: [góra], [dół], [lewa], [prawa]). Z każdą krawędzią związana jest lista liczb
-- całkowitych, określających ile piramid powinno być widocznych z danego miejsca w rzędzie 
-- lub kolumnie.
data Edges = Edges [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int] deriving Show

-- | Typ reprezentujący planszę łamigłówki. 
-- Plansza składa się z czterech opisanych krawędzi oraz dyskretnej, kwadratowej siatki 
-- pól o wymiarach N x N.
data Board = Board Edges [[Int]]

-- | Synonim typu dla pary liczb całkowitych, reprezentujący współrzędne planszy.
-- Pierwsza liczba pary określa numer wiersza, druga numer kolumny.
type Coordinates = (Int, Int)

-- | Zwraca pustą planszę z danym zestawem krawędzi.
-- Note: Puste pole reprezentowane jest jako piramida o wysokości 0.
emptyBoard :: Edges -> Board
emptyBoard edges = Board edges $ replicate boardSize $ replicate boardSize 0
    where 
        (Edges top _ _ _) = edges
        boardSize = length top

-- | Zwraca rozmiar planszy (liczbę rzędów lub kolumn).
sizeOf :: Board -> Int
sizeOf (Board _ rows) = length rows

-- | Umieszcza na polu planszy o podanych współrzędnych piramidę o danej wysokości.
place :: Board -> Coordinates -> Int -> Board
place (Board edges oldRows) (row, col) pyramid = 
    Board edges (rowsAbove ++ [changedRow] ++ rowsBelow)
    where
        (rowsAbove, rowToChange:rowsBelow) = splitAt row oldRows
        (colsOnLeft, _:colsOnRight) = splitAt col rowToChange
        changedRow = colsOnLeft ++ [pyramid] ++ colsOnRight

-- | Zwraca wartość pola planszy o podanych współrzędnych.
-- Wartością pola jest wysokość umieszczonej na nim piramidy lub zero, gdy pole jest puste.
getElem :: Board -> Coordinates -> Int
getElem (Board _ rows) (row, col) = (rows !! row) !! col

-- | Zwraca wiersz planszy o podanym numerze.
getRow :: Board -> Int -> [Int]
getRow (Board _ rows) rowNumber = rows !! rowNumber

-- | Zwraca kolumnę planszy o podanym numerze.
getCol :: Board -> Int -> [Int]
getCol (Board _ rows) colNumber = [ row !! colNumber | row <- rows ]

-- | Wyświetla planszę wraz z liczbami na jej krawędziach.
instance Show Board where
    show (Board (Edges top bottom left right) fields) = 
        unlines $ ([' ' : showEdge top] 
                ++ showSides left right (map showRow fields) 
                ++ [' ' : showEdge bottom])
        where
            showEdge []     = ""
            showEdge (x:xs) = ' ' : (showMaybe x) ++ showEdge xs
            showRow []     = "|"
            showRow (x:xs) = '|' : (show x) ++ showRow xs
            showSides []     []     []     = []
            showSides (l:ls) (r:rs) (x:xs) = [showMaybe l ++ x ++ showMaybe r] ++ showSides ls rs xs
            showMaybe Nothing  = " "
            showMaybe (Just x) = show x

