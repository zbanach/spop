{-|
Module      : Tests
Description : Testy automatyczne solvera łamigłówki "Piramidy"
Copyright   : Zbigniew Banach, Michał Breiter 2016

Moduł umożliwia automatyczne wykonanie testów poprawności działania algorytmu rozwiązywania
łamigłówki "Piramidy". Dla zadanych plików testowych wynik działania algorytmu porównywany 
jest z wzorcem rozwiązania zapisanym w oddzielnym pliku tekstowym. 

Testowe łamigłówki oraz ich rozwiązania wzorcowe pobrane zostały ze strony:
http://www.wydawnictwologi.pl/piramida
-}
module Tests where

import Prelude
import System.IO

import Pyramids
import Solver
import Main hiding (main)


-- | Zestaw plików testowych
-- Pliki powinny znajdować się w katalogu "test" i mieć rozszerzenie ".txt".
-- Nazwa pliku z rozwiązaniem wzorcowym musi mieć przyrostek "_solution".
testFiles = ["p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10"]

main = runTests testFiles 1

runTests []     _ = putStrLn "Wszystkie testy zaliczone pomyślnie"

runTests (t:ts) currentTest = do 
    let puzzleFile = "test/" ++ t ++ ".txt"
    let solutionFile = "test/" ++ t ++ "_solution.txt"
    putStr ("[" ++ (show currentTest) ++ ("/") ++ (show $ length testFiles) ++ "] ")
    putStr ("Test planszy " ++ puzzleFile ++ "... ")
    pyramids <- readPyramidsFile puzzleFile
    let solution = show $ solve pyramids
    expectedSolution <- readFile solutionFile
    if solution == expectedSolution then do
        putStrLn "OK"
        runTests ts (currentTest + 1)
    else do
        putStrLn " Błąd\n\nZnalezione rozwiązanie:"
        putStrLn solution
        putStrLn "Oczekiwane rozwiązanie:"
        putStrLn expectedSolution
        
    
