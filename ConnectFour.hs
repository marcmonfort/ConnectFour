


import Data.Char
import Text.Read (readMaybe)


type Board = [[Char]]

showTurn :: Board -> [Char]
showTurn board = 
    '\n':' ':' ':(foldr (\x acc -> x : ' ' : acc) [] numbers) ++
    showBoard board
    where
        numbers = map (intToDigit) (take size (iterate (+1) 1))
        size = length (head board)

showBoard :: Board -> [Char]
showBoard [] = []
showBoard board = 
    '\n':' ':(showRow(head board)) ++ 
    '\n':' ':box ++
    showBoard (tail board)
    where
        row = head board
        box = if length board == 1 
            then '╰':(concat (take ((length row)-1) (repeat "─┴"))) ++ "─╯"
            else '├':(concat (take ((length row)-1) (repeat "─┼"))) ++ "─┤"


showRow :: [Char] -> [Char]
showRow [] = '│':[] 
showRow (x:xs) = '│':x:showRow xs


emptyRow :: Int -> [Char]
emptyRow m = take m $ repeat ' '

emptyBoard :: Int -> Int -> Board
emptyBoard 0 _ = []
emptyBoard n m =  [emptyRow m] ++ emptyBoard (n-1) m
   
main :: IO ()
main = do
    putStrLn "\nBoard size:"

    putStr "  height = "
    height <- getLine

    putStr "  width  = "
    width <- getLine

    let n = readMaybe height :: Maybe Int
    let m = readMaybe width  :: Maybe Int
    if (n == Nothing || m == Nothing) then do
        putStr ("\nError: height and width should be integers\n")
        main
    else --do
        putStrLn $ showTurn $ emptyBoard (read height) (read width)

    --putStrLn "Choose computer strategy:\n  1. random\n  2. greedy\n  3. smart"
    --strategy <- getLine
    --putStrLn ("you choose: " ++ strategy)