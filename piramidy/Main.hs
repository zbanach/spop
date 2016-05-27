{-|
Module      : Main
Description : Aplikacja rozwiązująca łamigłówkę "Piramidy"
Copyright   : Zbigniew Banach, Michał Breiter 2016

Aplikacja znajduje i wyświetla rozwiązanie łamigłówki "Piramidy" (o ile istnieje).
Po uruchomieniu, aplikacja prosi użytkownika o podanie nazwy pliku wejściowego z łamigłówką.

Aplikacja została zrealizowana w ramach projektu z przedmiotu SPOP w semestrze 16L.
-}
module Main where

import Prelude
import System.IO
import Debug.Trace

import Pyramids
import Solver


-- | Typ reprezentujący dane wejściowe łamigłówki do rozwiązania.
data Piramidy = Piramidy [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int] deriving Read

-- | Konwertuje typ łamigłówki odczytany z pliku do typu oczekiwanego przez Solver.
convert (Piramidy top bottom left right) = Edges top bottom left right


main = do 
    putStrLn "Podaj nazwę pliku:"
    fileName <- getLine
    pyramids <- readPyramidsFile fileName
    let solution = solve pyramids
    printSolution solution

readPyramidsFile fileName = do
    content <- readFile fileName
    let pyramids = read content :: Piramidy
    return $ convert pyramids

printSolution NoSolution       = putStrLn "\nRozwiązanie nie istnieje\n"
printSolution (Solution board) = putStrLn $ "\nZnalezione rozwiązanie:\n" ++ show board

