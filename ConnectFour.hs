import System.IO

import Data.Char
import Data.Maybe (fromJust)
import Text.Read (readMaybe)


type Board = [[Char]]

data Strategy = Random | Greedy | Smart
    deriving (Eq)

data Player = AI | P1 | P2
    deriving (Eq,Show)


-- SHOW BOARD

showTurn :: Board -> [Char]
showTurn board = 
    '\n':' ':' ':(foldr (\x acc -> if x `mod` 10 == 0 then ((show x )!!0) : ' ' : acc else ' ' : ' ' : acc) [] numbers) ++
    '\n':' ':' ':(foldr (\x acc -> if x > 9 then ((show x )!!1) : ' ' : acc else ((show x )!!0) : ' ' : acc) [] numbers) ++
    showBoard board
    where
        numbers = (take size (iterate (+1) 1))
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

--EMPTY BOARD

emptyRow :: Int -> [Char]
emptyRow m = take m $ repeat ' '

emptyBoard :: Int -> Int -> Board
emptyBoard 0 _ = []
emptyBoard n m =  [emptyRow m] ++ emptyBoard (n-1) m

--PLAYING
-- Posa una fitxa al forat (r,c)
changeBoard :: Board -> Player -> Int -> Int -> Board
changeBoard board player c r = 
    replace r (replace c simbol (board !! r)) board 
    where
        simbol = case player of
            AI -> 'X'
            P1 -> 'O'
            P2 -> '▆'

-- Modifica l'element n de la llista
replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a:xs
replace n a (x:xs) = x:(replace (n-1) a xs)

-- Retorna la fila on caura la fitxa a la columna c
getRow :: Board -> Int -> Int
getRow board c = 
    length $ takeWhile (==' ') (foldr (\x acc -> x!!c : acc) [] board)

-- Comprova que la columna sigui valida
checkColumn :: Board -> Int -> Bool
checkColumn board c
    | c > length board = False
    | c < 1 = False
    | otherwise = True

play :: Board -> Player -> IO ()
play board player = do
    putStrLn $ showTurn $ board --mirar ganador o todo lleno...
    putStr " Your Turn\n Choose Column: "
    hFlush stdout
    input <- getLine
    let column = readMaybe input :: Maybe Int -- check interval ((if int and interval)
    case column of
        Nothing -> putStrLn "\n\n\t\x1b[31mERROR: Column should be integer\x1b[0m" >> play board player
        Just c -> do
            if not $ checkColumn board c then do
                putStrLn ("\n\n\t\x1b[31mERROR: Invalid column \x1b[0m" ++ (show c))
                play board player
            else do
                let r = getRow board (c-1) -- mirar que no sea cero!
                if r == 0 then do
                    putStrLn ("\n\n\t\x1b[31mERROR: No empty holes in column \x1b[0m" ++ (show c))
                    play board player
                else do
                    let next_board = changeBoard board player (c-1) (r-1)
                    if player == P1 then play next_board P2 else play next_board P1



main :: IO ()
main = do
    putStrLn "\nBoard size:"
    putStr "  height = "
    hFlush stdout

    height <- getLine
            --putStrLn $ "\x1b[32m" ++ "highlight me" ++ "\x1b[0m" ++ " but not me"
    putStr "  width  = "
    hFlush stdout
    width <- getLine

    let n = readMaybe height :: Maybe Int
    let m = readMaybe width  :: Maybe Int
    if (n == Nothing || m == Nothing) then do
        putStrLn ("\n\n\t\x1b[31mERROR: height and width should be integers\x1b[0m")
        main
    else do
        --putStrLn $ showTurn $ emptyBoard (read height) (read width)
        let str = Random
        play (emptyBoard (fromJust n) (fromJust m)) P1
        return ()


    --putStrLn "Choose computer strategy:\n  1. random\n  2. greedy\n  3. smart"
    --strategy <- getLine
    --putStrLn ("you choose: " ++ strategy)

--getNumber :: IO ()




