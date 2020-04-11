import System.IO

import Data.Char
--import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Data.List (find)
import System.Random



type Board = [[Char]]

data Strategy = Random | Greedy | Smart
    deriving (Eq)

data Player = AI | P1 | P2
    deriving (Eq,Show)


-- SHOW BOARD

showTurn :: Board -> [Char]
showTurn board = 
    "\n    " ++ (foldr (\x acc -> if x > 9 then (show x ) ++ "  " ++ acc else (show x ) ++ "   " ++ acc) [] numbers) ++
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
            then " ╰─" ++ (concat (take ((length row)-1) (repeat "──┴─"))) ++ "──╯" --" ╰─"
            else " ├─" ++ (concat (take ((length row)-1) (repeat "──┼─"))) ++ "──┤"

showRow :: [Char] -> [Char]
showRow [] = ' ':'│':[] 
showRow (x:xs) = " │ " ++ x:showRow xs

--EMPTY BOARD

emptyBoard :: Int -> Int -> Board
emptyBoard n m =  replicate n $ replicate m ' '

--PLAYING
-- Posa la fitxa del jugado player al forat (r,c)
changeBoard :: Board -> Player -> Int -> Int -> Board
changeBoard board player c r = 
    replace r (replace c simbol (board !! r)) board 
    where
        simbol = case player of
            AI -> 'O'
            P1 -> 'X'
            P2 -> 'X'

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
    | c > length (board!!0) = False
    | c < 1 = False
    | otherwise = True

-- CHECK FULL BOARD

isFull :: Board -> Bool
isFull board = all (/=' ') (board!!0)



-- RANDOM

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).

randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

dice :: IO ()
-- main program that throws two dice.

dice = do
    r1 <- randInt 1 6
    putStr (show [r1])


-- CHECK WINNER
checkWList :: [Char] -> Char
checkWList [] = '?'
checkWList list@(l:ls)
    | l /= ' ' && length list >= 4 && same4 = l
    | length list < 4 = '?'
    | otherwise = checkWList ls
    where
        same4 = foldl (\acc x -> acc && (x==l) ) True (take 4 list)
    
rrrr :: Board -> Char
rrrr [] = '?'
rrrr (x:xs)
    | winner /= '?' = winner
    | otherwise = rrrr xs
    where
        winner = checkWList x

cccc :: Board -> Int -> Char
cccc _ 0 = '?'
cccc board n
    | winner /= '?' = winner
    | otherwise = cccc board (n-1)
    where
        winner = checkWList (foldr (\x acc -> x!!(n-1) : acc) [] board)

dddd :: Board -> Int -> Char
dddd _ 0 = '?'
dddd board n
    | winner1 /= '?' = winner1
    | winner2 /= '?' = winner2
    | otherwise = dddd board (n-1)
    where
        winner1 = checkWList (foldr (\x acc -> if ((n-1-length acc)>=0 && ((n-1-length acc) < (length x)) ) then x!!(n-1-length acc) : acc else ' ':acc) [] board)
        winner2 = checkWList (foldr (\x acc -> if ((n-h+length acc)>=0 && ((n-h+length acc) < (length x)) ) then x!!(n-h+length acc) : acc else ' ':acc) [] board)
        h = length board

checkWinner :: Board -> Char
checkWinner board
    | rs /= '?' = rs
    | cs /= '?' = cs
    | ds /= '?' = ds
    | otherwise = '?'
    where 
        rs = rrrr board 
        cs = cccc board (length $ board!!0)
        ds = dddd board (length board + length (board!!0) -1)




-- PERSON MOVES

askColumn :: Board -> [Char] -> IO (Int,Int)
askColumn board msg = do
    putStrLn $ showTurn $ board
    putStr msg
    hFlush stdout
    input <- getLine
    let col = readMaybe input :: Maybe Int
    case col of
        Nothing -> putStrLn ("\t\x1b[31mERROR: Should be Integer\x1b[0m") >> askColumn board msg
        Just c -> 
            if not $ checkColumn board c then do
                putStrLn ("\t\x1b[31mERROR: Invalid column \x1b[0m") >> askColumn board msg
            else do
                let r = getRow board (c-1)
                if r == 0 then do
                    putStrLn ("\t\x1b[31mERROR: No empty holes in column \x1b[0m" ++ (show c)) >> askColumn board msg
                else
                    return (c,r)

showWinner :: Board -> Char -> IO ()
showWinner board winner
    | winner == 'X' = putStrLn $ (showTurn board) ++ "\n  Winner!!! Good Job " ++ (show winner)
    | winner == 'O' = putStrLn $ (showTurn board) ++ "\n  Game OVER! - You Lose! " ++ (show winner)


{- caca :: [Char] -> Int
caca [] = '?'
caca list@(l:ls)
    | l /= ' ' && length list >= 4 && same4 = l
    | length list < 4 = '?'
    | otherwise = caca ls
    where
        same4 = foldl (\acc x -> acc && (x==l) ) True (take 4 list) -}
nConnect :: [Char] -> Int -> Char -> Int
nConnect list c k = 1 + (length a) + (length b)
    where
        a = takeWhile (==k) (tail (reverse l1))
        b = takeWhile (==k) l2
        --k = list!!(c-1) -- poria poner un parametro como char de elemento...
        (l1,l2) = splitAt c list


maxCol :: Board -> Int -> Char -> (Int,Int)
maxCol board 0 k = (0,0)
maxCol board c k
    | r > 0 && mmm > m2 = (c,mmm)
    | otherwise = (c2,m2)
    where
        r = getRow board (c-1)
        lc = (foldr (\x acc -> x!!(c-1) : acc) [] board)
        ld = (foldr (\x acc -> if ((n-1-length acc)>=0 && ((n-1-length acc) < (length x)) ) then x!!(n-1-length acc) : acc else ' ':acc) [] board)
        ld2 = (foldr (\x acc -> if ((n2-h+length acc)>=0 && ((n2-h+length acc) < (length x)) ) then x!!(n2-h+length acc) : acc else ' ':acc) [] board)
        h = length board
        n = c + (h - r)
        n2 = c + (r-1)
        mmm = maximum [nConnect ld2 r k, nConnect ld r k, nConnect (board!!(r-1)) c k, nConnect lc r k]
        (c2,m2) = maxCol board (c-1) k


--evalBoard :: Board -> Int

--              color   depth  boardf 

{- abMinimax :: Cell -> Int -> Board -> (Maybe Int, Score)
abMinimax color depth board
  | depth <= 0 = (Nothing, scoreBoard board)
  | otherwise =
    let scoresMoves = [(score, move)
                       | move <- [0..width-1], not ((0,move) `member` board),
                         let board' = dropDisk board move color,
                         let score = snd . abMinimax (otherColor color) (depth-1) $ board']
    in case find ((== targetScore) . fst) scoresMoves of
          Just (killerScore,killerMove) -> (Just killerMove, killerScore)
          _ -> let (score,move) = optimum scoresMoves
               in (if debug && depth==maxDepth then trace' scoresMoves else ()) -- just for debugging
                  `seq` (Just move, score)
  where trace' [] = ()
        trace' ((score,move):rest) = trace ("Depth "++(show depth)++", placing on "++(show move)++", score "++(show score))
                                           (trace' rest)
        (targetScore,optimum) = case color of
              Orange -> (orangeWins, maximum)
              Yellow -> (yellowWins, minimum) -}

-- Board Player -> Int -> (Mayb.e..)
--      top         botom
lili :: [Char] -> [Char] -> Int
lili [] _ = 0
lili _ [] = 0
lili list1@(l1:l1s) list2@(l2:l2s)
    | x && t0 && a = 1000 
    | x && t0 && b = -1000
    | otherwise = lili l1s l2s
    where
        x = length list1 >= 5
        cut1 = take 5 list1
        cut2 = take 5 list2
        m = tail $ init cut1
        a = all (=='O') m
        b = all (=='X') m

        t0 = t1 && t2
        t1 = head cut1 == ' ' && last cut1 == ' '
        t2 = head cut2 /= ' ' && last cut2 /= ' '

extraScore :: Board -> Int
extraScore [] = 0
extraScore (x:xs)
    | extra == 1000 || extra == -1000 = extra
    | otherwise = extraScore xs
    where
        extra = lili x bottom
        bottom = if length xs == 0 then replicate (length x) 'N' else head xs


coco :: [Char] -> Int
coco [] = 0
coco list@(l:ls)
    | numAI == 4 = 1000
    | numP1 == -4 = -1000
    | cc == 1000 || cc == -1000 = coco ls
    | x && a && not b && n = numAI + cc
    | x && b && not a && n = numP1 + cc
    | otherwise = cc
    where
        cuatro = take 4 list
        x = length list >= 4
        a = elem 'O' cuatro
        b = elem 'X' cuatro
        n = not (elem 'N' cuatro)
        numAI = length (filter (=='O') cuatro)
        numP1 = -(length (filter (=='X') cuatro))
        cc = coco ls




rowScore :: Board -> Int
rowScore [] = 0
rowScore (x:xs)
    | ar == 1000 || ar == -1000 = ar
    | rrs == 1000 || rrs == -1000 = rrs
    | otherwise = ar + rrs
    where
        ar = coco x
        rrs = rowScore xs


colScore :: Board -> Int -> Int
colScore _ 0 = 0
colScore board n
    | cs == 1000 || cs == -1000 = cs
    | bs == 1000 || bs == -1000 = bs
    | otherwise = cs + bs
    where
        column = (foldr (\x acc -> x!!(n-1) : acc) [] board)
        cs = coco column
        bs = colScore board (n-1)

diaScore :: Board -> Int -> Int
diaScore _ 0 = 0
diaScore board n
    | ds1 == 1000 || ds1 == -1000 = ds1
    | ds2 == 1000 || ds2 == -1000 = ds2
    | bs == 1000 || bs == -1000 = bs
    | otherwise = ds1 + ds2 + bs
    where
        diag1 = (foldr (\x acc -> if ((n-1-length acc)>=0 && ((n-1-length acc) < (length x)) ) then x!!(n-1-length acc) : acc else 'N':acc) [] board)
        diag2 = (foldr (\x acc -> if ((n-h+length acc)>=0 && ((n-h+length acc) < (length x)) ) then x!!(n-h+length acc) : acc else 'N':acc) [] board)
        ds1 = coco diag1
        ds2 = coco diag2
        bs = diaScore board (n-1)
        h = length board

scoreBoard :: Board -> Int
scoreBoard board 
    | rs == -1000 || cs == -1000 || ds == -1000 = -1000
    | rs == 1000 || cs == -1000 || ds == 1000 = 1000
    | cs == 1000 || cs == -1000 = cs
    -- | ds == 1000 || ds == -1000 = ds
    | extra == 1000 || extra == -1000 = extra
    | otherwise = total-- + extra
    where 
        rs = rowScore board 
        cs = colScore board (length $ board!!0)
        ds = diaScore board (length board + length (board!!0) -1)
        extra = extraScore board
        total = rs + cs -- + ds



otherColor :: Player -> Player
otherColor AI = P1
otherColor P1 = AI
otherColor P2 = AI

abMinimax :: Player -> Int -> Board -> (Maybe Int, Int)
abMinimax color depth board
    | depth <= 0 = (Nothing, scoreBoard board)
    | otherwise =
        let scoresMoves = [(score, c)
                            | c <- [1..width],
                            --let width = length (board!!0),         
                            let r = getRow board (c-1), -- puede que mejor hacerlo en restrc
                            let board' = changeBoard board color (c-1) (r-1),
                            r > 0,
                            let score = snd . abMinimax (otherColor color) (depth-1) $ board']
        in case find ((== targetScore) . fst) scoresMoves of
            Just (killerScore,killerMove) -> (Just killerMove, killerScore)
            _ -> if length scoresMoves /= 0 then let (score,c) = optimum scoresMoves in (Just c, score) else (Nothing,0)
    where
        (targetScore,optimum) = case color of
            AI -> (1000, maximum)
            P1 -> (-1000, minimum)
            P2 -> (-1000, minimum)
        width = length $ board!!0


--(AIWin,PWin) = (1000000,-AIWin)



--minimax :: Board -> Depth -> 



moveAI :: Board -> Strategy -> IO (Int,Int)
moveAI board Random = do
    c <- randInt 1 (length (board!!0))
    let r = getRow board (c-1)
    if r == 0 then
        moveAI board Random
    else return (c,r)
moveAI board Greedy = do
    let (c,m) = (maxCol board (length (board!!0)) 'O')    --error ¿i si no hay huecos?
    let (c2,m2) = (maxCol board (length (board!!0)) 'X') 
    if m2 == 4 && m /= 4 then do
        let r = getRow board (c2-1)
        return (c2,r)
    else do
        let r = getRow board (c-1)
        return (c,r)
moveAI board Smart = do
    let (mv, ooo) = abMinimax AI 4 board
    print((mv,ooo))
    case mv of
        Just col -> do
            let r = getRow board (col-1)
            return (col,r)
        _ -> putStrLn "No move possible" >> return (1,1)

-- extra AI vs AI
moveP2 board Smart = do
    let (mv, ooo) = abMinimax P2 6 board
    print((mv,ooo))
    case mv of
        Just col -> do
            let r = getRow board (col-1)
            return (col,r)
        _ -> putStrLn "No move possible" >> return (1,1)



play :: Board -> Player -> Strategy -> IO ()
play board P1 str
    | winner /= '?' = showWinner board winner >> main
    | isFull board = putStrLn ((showTurn board) ++ "\n  That's a TIE") >> main
    | otherwise = do
        --putStrLn $ showTurn $ board 
        (c,r) <- askColumn board "  Your Turn\n  Choose Column: "
        let next_board = changeBoard board P1 (c-1) (r-1)
        --print (scoreBoard2 board)
        play next_board AI str
    where
        winner = checkWinner board
play board AI str
    | winner /= '?' = showWinner board winner >> main
    | isFull board = putStrLn ((showTurn board) ++ "\n  That's a TIE") >> main
    | otherwise = do
        putStrLn $ showTurn board ++ "\n  AI Turn\n  Thinking... "
        (c,r) <- moveAI board str
        putStrLn $ "\n  Column -> " ++ (show c)

        let next_board = changeBoard board AI (c-1) (r-1)
        play next_board P1 str
        --play next_board P2 str  
    where
        winner = checkWinner board
play board P2 str
    | winner /= '?' = showWinner board winner >> main
    | isFull board = putStrLn ((showTurn board) ++ "\n  That's a TIE") >> main
    | otherwise = do
        putStrLn $ showTurn board ++ "\n  P2 Turn\n  Thinking... "
        (c,r) <- moveP2 board str
        putStrLn $ "\n  Column -> " ++ (show c)

        let next_board = changeBoard board P2 (c-1) (r-1)
        play next_board AI str  
    where
        winner = checkWinner board



askInt :: [Char] -> IO Int
askInt msg = do
    putStr msg
    hFlush stdout
    input <- getLine
    let num = readMaybe input :: Maybe Int
    case num of
        Nothing -> putStrLn "\n\t\x1b[31mERROR: Should be Integer\x1b[0m" >> askInt msg
        Just n -> 
            if n < 1 then do
                putStrLn ("\n\t\x1b[31mERROR: Should be greater than zero\x1b[0m") >> askInt msg
            else return n

main :: IO ()
main = do 
    putStrLn "\n\t ¡New Game!\n\t————————————\n Board size:"

    n <- askInt "   height = "
    m <- askInt "   width  = "
    let str = Smart
    play (emptyBoard n m) P1 str


         

    --putStrLn "Choose computer strategy:\n  1. random\n  2. greedy\n  3. smart"
    --strategy <- getLine
    --putStrLn ("you choose: " ++ strategy)

--getNumber :: IO ()




