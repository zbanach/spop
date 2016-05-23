
import Prelude
import System.IO
import Debug.Trace
import Data.List

data Piramidy = Piramidy [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int] deriving (Show, Read)

data Plansza = Plansza [[Int]] deriving (Show, Read)

-- | Zwróć rozmiar planszy do gry
-- Note: plansza jest symetryczna
sizeOfPiramidy :: Piramidy -> Int
sizeOfPiramidy (Piramidy x _ _ _) = length x

sizeOfBoard :: Plansza -> Int
sizeOfBoard (Plansza board) = length board 

-- | Zwróć nową planszę ze zmienioną jedną komórką
setElem :: Plansza -> (Int,  Int) -> Int -> Plansza
setElem (Plansza old) (row, col) newValue = 
    Plansza (rowsTop ++ [changed] ++ rowsBottom)
    where
        (rowsTop, rowToChange:rowsBottom) = splitAt row old
        (colsLeft, _:colsRight) = splitAt col rowToChange
        changed = colsLeft ++ [newValue] ++ colsRight
        
-- | Zwróć element planszy
getElem :: Int     -- ^ wiersz
        -> Int     -- ^ kolumna
        -> Plansza -- ^ plansza
        -> Int
getElem row col (Plansza board) = (board !! row) !! col

-- | Zwróć kolumnę planszy
getCol :: Int     -- ^ kolumna
       -> Plansza -- ^ plansza
       -> [Int]
getCol col (Plansza board) = [ row !! col | row <- board ] 

-- | Zwróć wiersz planszy
getRow row (Plansza board) = board !! row

-- | Ustaw element w liscie
setListElem :: [a]
        -> Int  -- ^ indeks
        -> a  -- ^ nowa wartosc
        -> [a]
setListElem oldList index newValue =
    begin ++ [newValue] ++ end
    where
        (begin, _:end) = splitAt index oldList


-- | Zwraca True jeśli piramida o wysokości 'pyramid' może być umieszczona na polu planszy o współrzędnych (x, y).
-- Piramida może być umieszczona na planszy jeżeli:
--  * w danym wierszu i kolumnie nie ma już piramidy o tej samej wysokości
--  * po umieszczeniu piramidy nie będą naruszone warunki łamigłówki (widoczna będzie odpowiednia liczba piramid)
canBePlaced :: Piramidy -> Plansza -> (Int, Int) -> Int -> Bool
canBePlaced sides board (row, col) pyramid = 
    isOnlyOneInRowAndColumn board (row, col) pyramid
    && fulfillsPyramidCondition sides board (row, col) pyramid
    
isOnlyOneInRowAndColumn board (row, col) pyramid =
    not $ or $ map (\i -> (getElem i col board == pyramid)
        || (getElem row i board == pyramid)
        )
        [0..(sizeOfBoard board) - 1] 
        
replaceIfZero [] [] = []
replaceIfZero (0:xs) (y:ys) = y:replaceIfZero xs ys
replaceIfZero (x:xs) y = x:replaceIfZero xs y

insertPyramidAndReplaceZero lst place pyramid ascending = 
    replaceIfZero newList remaining
    where
        remaining = [ x | x <- range, not $ x `elem` newList]
        range = if ascending then [1..n] else [n, n - 1..1]
        newList = setListElem lst place pyramid
        n = length lst

visibleFromTop board Nothing (row, col) pyramid = True
visibleFromTop board (Just target) (row, col) pyramid =
    minVisible <= target && target <= maxVisible
    where
        pyramidsFromTop = getCol col board
        minVisible = countVisiblePyramids $ insertPyramidAndReplaceZero pyramidsFromTop row pyramid False
        maxVisible = countVisiblePyramids $ insertPyramidAndReplaceZero pyramidsFromTop row pyramid True

visibleFromBottom board Nothing (row, col) pyramid = True
visibleFromBottom board (Just target) (row, col) pyramid =
   minVisible <= target && target <= maxVisible
   where
       pyramidsFromBottom = reverse $ getCol col board
       position = length pyramidsFromBottom - row - 1
       minVisible = countVisiblePyramids $ insertPyramidAndReplaceZero pyramidsFromBottom position pyramid False
       maxVisible = countVisiblePyramids $ insertPyramidAndReplaceZero pyramidsFromBottom position pyramid True

visibleFromLeft board Nothing (row, col) pyramid = True
visibleFromLeft board (Just target) (row, col) pyramid =
   minVisible <= target && target <= maxVisible
   where
       pyramidsFromLeft = getRow row board
       minVisible = countVisiblePyramids $ insertPyramidAndReplaceZero pyramidsFromLeft col pyramid False
       maxVisible = countVisiblePyramids $ insertPyramidAndReplaceZero pyramidsFromLeft col pyramid True
       
visibleFromRight board Nothing (row, col) pyramid = True
visibleFromRight board (Just target) (row, col) pyramid =
  minVisible <= target && target <= maxVisible
  where
      pyramidsFromRight = reverse $ getRow row board
      position = length pyramidsFromRight - col - 1
      minVisible = countVisiblePyramids $ insertPyramidAndReplaceZero pyramidsFromRight position pyramid False
      maxVisible = countVisiblePyramids $ insertPyramidAndReplaceZero pyramidsFromRight position pyramid True
     
fulfillsPyramidCondition sides board (row, col) pyramid =
    visibleFromTop board (top !! col) (row, col) pyramid
    && visibleFromBottom board (bottom !! col) (row, col) pyramid
    && visibleFromLeft board (left !! row) (row, col) pyramid
    && visibleFromRight board (right !! row) (row, col) pyramid
    where
        (Piramidy top bottom left right) = sides
    
countVisiblePyramids lst = 
    countVisiblePyramids' lst 0
    where
        countVisiblePyramids' [] _ = 0
        countVisiblePyramids' (x:xs) max = case x > max of
            True -> 1 + countVisiblePyramids' xs x
            otherwise -> countVisiblePyramids' xs max

nextCell sides board (row, col)
    | col == n - 1 && row == n - 1 = Just board
    | col == n - 1                 = solve' sides board (row + 1, 0)
    | otherwise                    = solve' sides board (row, col + 1)   
    where
        n = sizeOfBoard board
        
tryPyramids 0 _ _ _ = Nothing
tryPyramids n sides board (row, col) = 
    if canBePlaced sides board (row, col) n then
        case nextCell sides newBoard (row, col) of
            Nothing -> nextPyramid
            a -> a -- TODO to chyba mozna ladniej
    else
        nextPyramid
    where
        nextPyramid = tryPyramids (n - 1) sides board (row, col)
        newBoard = setElem board (row, col) n

solve' sides board (row, col) = case getElem row col board of
    0 -> tryPyramids (sizeOfBoard board) sides board (row, col)
--    otherwise -> nextCell sides board (row, col) -- TODO po co to?

solve :: Piramidy -> Maybe Plansza
solve sides = solve' sides startingBoard (0, 0)
    where
        startingBoard = Plansza $ replicate boardSize $ replicate boardSize 0
        boardSize = sizeOfPiramidy sides
    
    
readAndSolve fileName = do
    contents <- readFile fileName
    let sides = read contents :: Piramidy
    putStrLn $ "file " ++ fileName ++ " content:"
    putStrLn contents
    let solution = solve sides
    putStrLn "solution:"
    putStrLn $ show solution
