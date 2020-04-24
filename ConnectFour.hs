import System.IO (hFlush, stdout)   -- utilitzat per mostra per pantalla missatges sense haver de posar un salt de línia.
import Text.Read (readMaybe)        -- utilitzat pel tractament d'errors.
import Data.List (find)             -- utilitzat en lheurístic per simplificar el codi
import System.Random                -- utilitzat per la estratègia Random.


{- El codi està implementat amb blocs de funcions d'objectius similars.
En aquests blocs, les funcions més generals estan a sota, i les més especifiques es troben més amunt. 
Recomano llegir el codi començant des del main (a sota de tot), i anar pujant.-}

{- Board serà un tipus sinònim a una matriu de Char (vector de Strings).
Aporta claredat en definir les funcions -}
type Board = [[Char]]

{- Tipus enumerat per definir les 3 estratègies.
Ens facilita la desconstrucció de les funcions amb patrons i aporta claredat. -}
data Strategy = Random | Greedy | Smart
    deriving (Eq)

{- Tipus enumerat per definir els Jugadors.
Aporta claredat en les funcions, i la deconstrucció en patrons. -}
data Player = AI | P1 | AI2
    deriving (Eq)




-- -- -- -- -- -- -- --
-- -- SHOW BOARD  -- --
-- -- -- -- -- -- -- --


{- Retorna un String del tauler i el número de cada columna, per poder representar-ho per pantalla -}
showTurn :: Board -> [Char]
showTurn board = 
    "\n    " ++ (foldr (\x acc -> if x > 9 then (show x ) ++ "  " ++ acc else (show x ) ++ "   " ++ acc) [] numbers) ++     -- Número de cada columna. Els espais depenen del nombre de dígits.
    showBoard board
    where
        numbers = (take size (iterate (+1) 1))      -- llista dels números de les columnes.
        size = length (head board)                  -- número total de columnes.


{- Converteix el Board amb un String representable per pantalla de forma còmode. -}
showBoard :: Board -> [Char]
showBoard [] = []
showBoard (r:rs) = 
    '\n':' ':(showRow(r)) ++
    '\n':' ':box ++
    showBoard rs
    where
        box = if length rs == 0 
            then " ╰─" ++ (concat (take ((length r)-1) (repeat "──┴─"))) ++ "──╯"           -- part final del tauler
            else " ├─" ++ (concat (take ((length r)-1) (repeat "──┼─"))) ++ "──┤"           -- dibuix del tauler


{- Converteix una fila del tauler en un String per ser representat per pantalla -}
showRow :: [Char] -> [Char]
showRow [] = ' ':'│':[] 
showRow (x:xs) = " │ " ++ token:showRow xs
    where
        token = case x of       -- Modifiquem les fitxes perquè siguin més còmodes visualment
            'O' -> '●' 
            'X' -> '○' 
            _ -> x



-- -- -- -- -- -- -- -- --
-- --  CHECK WINNER  -- --
-- -- -- -- -- -- -- -- --


{- Retorna la fitxa guanyadora a la llista d'entrada. Si cap, retorna '?'.
Comprova per totes les combinacions de quatre posiciones juntes, si hi ha quatre fitxes d'un mateix color. -}
checkWList :: [Char] -> Char
checkWList [] = '?'
checkWList list@(l:ls)
    | length list < 4 = '?'                         -- Si hi ha menys de quatre posicions a la llista, ja no pot haver guanyador.
    | l /= ' ' && same4 = l                         -- Si hi ha quatre fitxes iguals juntes, retornem aquesta fitxa guanyadora. 
    | otherwise = checkWList ls                     -- Si no hi ha guanyador, i encara queden fitxes, mirem les següents posicions.
    where
        same4 = foldl (\acc x -> acc && (x==l) ) True (take 4 list)     -- True si totes les fitxes de les 4 següents posicions són iguals.


{- Retorna la fitxa guanyadora les files. Si cap, retorna '?'
De forma recursiva comprovem totes les files del tauler.
Anem enviant cada fila a la funció checkWList per comprovar si hi ha quatre fitxes en ratlla. -}
rrrr :: Board -> Char
rrrr [] = '?'
rrrr (x:xs)
    | winner /= '?' = winner            -- si hi ha guanyador, retorna la fitxa guanyadora.
    | otherwise = rrrr xs               -- si cap guanyador, mirem les seguents files.
    where
        winner = checkWList x           -- comprova guanyador a la fila x


{- Retorna la fitxa guanyadora a les columnes. Si cap, retorna '?'.
De forma recursiva comprovem totes les columnes del tauler.
La funció "foldr" ens crea una llista per la diagonal de la columna "n"
Aquesta llista és enviada a la funció checkWList per comprovar is hi ha quatre fitxes en ratlla. -}
cccc :: Board -> Int -> Char
cccc _ 0 = '?'
cccc board n
    | winner /= '?' = winner            -- si hi ha guanyador, retorna la fitxa guanyadora.
    | otherwise = cccc board (n-1)      -- si cap guanyador, mirem les seguents columnes
    where
        winner = checkWList (foldr (\x acc -> x!!(n-1) : acc) [] board)     -- comprova guanyador a la columna "n"


{- Retorna la fitxa guanyadora a les diagonals. Si cap, retorna '?'.
De forma recursiva anem comprovant totes les diagonals del tauler.
Amb la funció "foldr" hem creat una llista per cada diagonal creixent i decreixent.
Aquestes llistes són enviades a la funció checkWList per comprovar si hi ha quatre fitxes en ratlla-}
dddd :: Board -> Int -> Char
dddd _ 0 = '?'
dddd board n
    | winner1 /= '?' = winner1          -- si hi ha guanyador, retorna la fitxa guanyadora.
    | winner2 /= '?' = winner2
    | otherwise = dddd board (n-1)      -- si cap guanyador, mirem les seguents diagonals.
    where
        winner1 = checkWList (foldr (\x acc -> if ((n-1-length acc)>=0 && ((n-1-length acc) < (length x)) ) then x!!(n-1-length acc) : acc else ' ':acc) [] board)  -- comprova guanyador diagonal decreixent "n"
        winner2 = checkWList (foldr (\x acc -> if ((n-h+length acc)>=0 && ((n-h+length acc) < (length x)) ) then x!!(n-h+length acc) : acc else ' ':acc) [] board)  -- comprova guanyador diagonal creixent "n"
        h = length board


{- Retorna la fitxa que ha guanya la partida. Si cap ha guanyat, retorna '?'.
El que fem és comprovar totes les combinacions de quatre posicions juntes.
Ho fem primer per les files, després per les columnes i finalment per les dues diagonals. -}
checkWinner :: Board -> Char
checkWinner board
    | rs /= '?' = rs
    | cs /= '?' = cs
    | ds /= '?' = ds
    | otherwise = '?'
    where 
        rs = rrrr board                                             -- comprova guanyador files
        cs = cccc board (length $ board!!0)                         -- comprova guanyador columnes
        ds = dddd board (length board + length (board!!0) -1)       -- comprova guanyador diagonals



-- -- -- -- -- -- -- --
-- --  AI RANDOM  -- --
-- -- -- -- -- -- -- --


{- Retorna un enter random entre els dos intervals -}
randInt :: Int -> Int -> IO Int
randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result



-- -- -- -- -- -- -- --
-- --  AI GREEDY  -- --     
-- -- -- -- -- -- -- --  


{- Retorna el maxim nombre de fitxes que podem concatenar del color indicat, en la llista i la posició indicada.-}
nConnect :: [Char] -> Int -> Char -> Int
nConnect list pos color = 1 + (length a) + (length b)   -- suma del nombre de fitxes a l'esquerra i a la dreta, més la fitxa que posem.
    where
        a = takeWhile (==color) (tail (reverse left))   -- llista esquerra de fitxes del mateix color
        b = takeWhile (==color) right                   -- llista dreta de fitxes del mateix color
        (left,right) = splitAt pos list                 -- dividim la llista per la posició on posarem la nova fitxa


{- Retorna la columna on es pot concatenar el màxim nombre de fitxes del color corresponent, i el valor del màxim.
Mirem per cada columna, la posició on caura la fitxa. En aquesta posició mirem el nombre de fitxes que podem concatenar per 
la fila, la columna, i les dues diagonals. Retornem la columna que pot concatenar més fitxes. -}
maxCol :: Board -> Int -> Char -> (Int,Int)
maxCol board 0 color = (0,0)
maxCol board c color
    | r > 0 && mmm > m2 = (c,mmm)   -- si la columna c obte el maxim, retornem aquest.
    | otherwise = (c2,m2)           -- sinó, retornem el màxim de la resta de columnes.
    where
        r = getRow board (c-1)
        lc = (foldr (\x acc -> x!!(c-1) : acc) [] board)    -- llista de la columna "c"
        ld = (foldr (\x acc -> if ((n-1-length acc)>=0 && ((n-1-length acc) < (length x)) ) then x!!(n-1-length acc) : acc else ' ':acc) [] board)  -- llista de la diagonal decreixent n
        ld2 = (foldr (\x acc -> if ((n2-h+length acc)>=0 && ((n2-h+length acc) < (length x)) ) then x!!(n2-h+length acc) : acc else ' ':acc) [] board)  -- llista de la diagonal creixent n2
        h = length board
        n = c + (h - r)     -- corespondència de la posició (c,r) amb les diagonals creixents i decreixents en aquell punt
        n2 = c + (r-1)
        mmm = maximum [nConnect ld2 r color, nConnect ld r color, nConnect (board!!(r-1)) c color, nConnect lc r color]     -- nombre màxim de fitxes que puc concatenar a la columna "c",
        (c2,m2) = maxCol board (c-1) color      -- calumna i valor màxim de la resta de les columnes.                       -- mirant files, columnes i les dos diagonals.



-- -- -- -- -- -- --
-- -- AI SMART -- --
-- -- -- -- -- -- --


{- Retorna 500/-500 segons si existeix la situació especial de victòria amb 3 fitxes
en la llista de sobre tenint en compte també com es la llista de sota. -}
listExtraScore :: [Char] -> [Char] -> Int
listExtraScore [] _ = 0
listExtraScore _ [] = 0
listExtraScore ltop@(l1:l1s) lbottom@(l2:l2s)
    | min5 && t0 && o = 500         -- situació especial, retornem 500/-500
    | min5 && t0 && x = -500
    | otherwise = listExtraScore l1s l2s
    where
        min5 = length ltop >= 5     -- mínim 5 posiciones pel cas especial
        cut1 = take 5 ltop
        cut2 = take 5 lbottom

        midle = tail $ init cut1    -- tres fitxes d'en mig
        o = all (=='O') midle
        x = all (=='X') midle

        t0 = t1 && t2               -- es pot posar fitxa en tots dos costats de les 3 d'en mig
        t1 = head cut1 == ' ' && last cut1 == ' '
        t2 = head cut2 /= ' ' && last cut2 /= ' '


{- Retorna 500 si el Board es troba en la situació especial de victòria amb 3 fitxes.
Si hi ha 3 fitxes d'un mateix color en ratlla, i la possibilitat de posar una quarta en els dos costats,
podem assegurar que en dos torns el jugador de les 3 fitxes haurà guanyat, ja que el contrari no podrà bloquejar els dos costats.
(sense tenir en compte la possibilitat que el contrari guanyi al següent torn, per això posem 500, i no 1000) -}
extraScore :: Board -> Int
extraScore [] = 0
extraScore (x:xs)
    | extra == 500 || extra == -500 = extra     -- Si trobem la situació retornem 500/-500 (segons color fitxa)
    | otherwise = extraScore xs
    where
        extra = listExtraScore x bottom
        bottom = if length xs == 0 then replicate (length x) 'N' else head xs   -- Fila de sota. Si no existeix, creem una amb tot "N" com si estigués plena.


{- Retorna l'heurístic de la llista.
La llista es pot considerar com una fila, una columna o una diagonal.
Per cada combinació de quatre caselles juntes, 
    sumem 3 si hi ha 3 caselles de l'AI i cap mes, 
    sumem 2 si hi ha 2 caselles de l'AI i cap mes,
    sumem 1 si hi ha 1 casella de l'AI i cap mes,
(El mateix per les caselles de P1 però en negatiu)
Si hi ha 4-en-ratlla, el valor de l'heurístic serà infinit (1000) per AI i -infinit (-1000) per P1
-}
listScore :: [Char] -> Int
listScore [] = 0
listScore list@(l:ls)
    | numAI == 4 = 1000                         -- valor infinit si hi ha quatre en ratlla en les quatre següents posicions.
    | numP1 == 4 = -1000
    | hs == 1000 || hs == -1000 = listScore ls  -- valor infinit si hi ha quatre en ratlla al la resta de la llista
    | min4 && o && not x && n = numAI + hs      -- sumem +(numero_fitxes) si hi ha només fitxes de AI
    | min4 && x && not o && n = -numP1 + hs     -- sumem -(numero_fitxes) si hi ha només fitxes de P1
    | otherwise = hs                            -- si no hi ha cap fitxa, o hi ha fitxes dels dos jugadors, recursivament calculem l'heurístic de la següent combinació de quatre caselles.
    where
        cuatro = take 4 list                    -- següent combinació de quatre caselles
        min4 = length list >= 4
        o = elem 'O' cuatro
        x = elem 'X' cuatro
        n = not (elem 'N' cuatro)               -- no contem les "N" en la generació de llistes diagonals
        numAI = length (filter (=='O') cuatro)
        numP1 = length (filter (=='X') cuatro)
        hs = listScore ls                       -- valor de l'heurístic del la resta de la llista.


{- Retorna el valor de l'heurístic de les files -}
rowScore :: Board -> Int
rowScore [] = 0
rowScore (x:xs)
    | rs == 1000 || rs == -1000 = rs        -- si alguna fila té el valor màxim/mínim, retornem aquest.
    | bs == 1000 || bs == -1000 = bs
    | otherwise = rs + bs
    where
        rs = listScore x    -- heurístic de la fila
        bs = rowScore xs    -- heurístic del resta de files


{- Retorna el valor del heurístic de les columnes -}
colScore :: Board -> Int -> Int
colScore _ 0 = 0
colScore board n
    | cs == 1000 || cs == -1000 = cs        -- si alguna columna té el valor màxim/mínim, retornem aquest.
    | bs == 1000 || bs == -1000 = bs
    | otherwise = cs + bs
    where
        column = (foldr (\x acc -> x!!(n-1) : acc) [] board)    -- llista de la columna "n"
        cs = listScore column       -- heurístic de la columna
        bs = colScore board (n-1)   -- heurístic de la resta de columnes


{- Retorna el valor del heurístic de les diagonals -}
diaScore :: Board -> Int -> Int
diaScore _ 0 = 0
diaScore board n
    | ds1 == 1000 || ds1 == -1000 = ds1     -- si alguna diagonal té el valor màxim/mínim, retornem aquest.
    | ds2 == 1000 || ds2 == -1000 = ds2
    | bs == 1000 || bs == -1000 = bs
    | otherwise = ds1 + ds2 + bs
    where
        diag1 = (foldr (\x acc -> if ((n-1-length acc)>=0 && ((n-1-length acc) < (length x)) ) then x!!(n-1-length acc) : acc else 'N':acc) [] board)   -- llista diagonal decreixent "n"
        diag2 = (foldr (\x acc -> if ((n-h+length acc)>=0 && ((n-h+length acc) < (length x)) ) then x!!(n-h+length acc) : acc else 'N':acc) [] board)   -- llista diagonal creixent "n"
        ds1 = listScore diag1       -- heurístic de la diagonal.
        ds2 = listScore diag2
        bs = diaScore board (n-1)   -- heurístic de la resta de diagonals
        h = length board


{- Heurístic que calcula el valor del tauler.-}
heuristicBoard :: Board -> Int
heuristicBoard board 
    | rs == 1000 || cs == 1000 || ds == 1000 = 1000                 -- Si temim un valor màxim o mínim, retornem aquest valor.
    | rs == -1000 || cs == -1000 || ds == -1000 = -1000
    | otherwise = total + extra
    where 
        rs = rowScore board                                         -- valor heurístic de les files.
        cs = colScore board (length $ board!!0)                     -- valor heurístic de les columnes
        ds = diaScore board (length board + length (board!!0) -1)   -- valor heurístic de les diagonals
        extra = extraScore board                                    -- valor heurístic de situacions especials.
        total = rs + cs + ds


{- Retorna el jugador del següent torn -}
nextPlayer :: Player -> Player
nextPlayer AI = P1
nextPlayer P1 = AI
nextPlayer AI2 = AI


{- Funció minimax. Retorna la columna que obte el millor valor segons la heurística heuristicBoard. -}
minimax :: Board -> Player -> Int -> (Maybe Int, Int)
minimax board player depth 
    | depth <= 0 = (Nothing, heuristicBoard board)                                              -- cas base. Calculem el valor del tauler. Encara no sabem quina és la columna, per això Nothing.
    | otherwise =
        let validMoves = [ (c,r) | c <- [1..width], let r = getRow board (c-1), r > 0]          -- llista posibles columnes on posar una nova fitxa.
        in case validMoves of
            []  -> (Nothing, heuristicBoard board)                                              -- Si la llista es buida, retornem el valor del tauler actual.
            _   ->
                let movesBoards = [(c, board_c) | (c,r) <- validMoves, let board_c = changeBoard board player (c-1) (r-1)]    -- llista dels taulers amb la nova fitxa.
                --in let theScores = [(c, heuristicBoard b) | (c,b) <- movesBoards ]
                in case find ((== endScore) . heuristicBoard . snd) movesBoards of
                    Just (endMove, _ ) -> (Just endMove, endScore)                          -- si existeix un tauler on ja hem guanyat, retornem la columna guanyadora.
                    _ -> 
                        let moveScore = [(score, c) | (c, board_c) <- movesBoards, let score = snd $ minimax board_c (nextPlayer player) (depth-1) ]    --llista dels valors de cada nou tauler
                        in let (score,c) = getMiniMax moveScore in (Just c, score)          -- retornem la columna amb el mínim/màxim score.
                            
    where
        (endScore,getMiniMax) = case player of      -- considerem 1000 com victòria per l'AI, i -1000 com victoria per P1.
            AI -> (1000, maximum)                   -- L'AI intenta maximitzar el valor de l'heurístic, i P1 intenta minimitzar-lo.
            P1 -> (-1000, minimum)
            AI2 -> (-1000, minimum)
        width = length $ board!!0



-- -- -- -- -- -- -- --
-- -- MOVIMENT AI -- --
-- -- -- -- -- -- -- --


{- Donat un tauler, retorna la posició de la següent fitxa segons l'estratègia.
Si és "smart", tindrem en compte la dificultat seleccionada, que serà la profunditat del minimax -}
moveAI :: Board -> Strategy -> Int -> IO (Int,Int)
moveAI board Random _ = do
    c <- randInt 1 (length (board!!0))  -- obtenim una columna de forma aleatoria.
    let r = getRow board (c-1)
    if r == 0 then
        moveAI board Random 0           -- si no hi ha cap forat a la columna, obtenim una nova columna.
    else return (c,r)

moveAI board Greedy _ = do
    let (c,m) = (maxCol board (length (board!!0)) 'O')      -- Obtenim la columna que ens permet concatenar el màxim nombre de fitxes (m).
    let (c2,m2) = (maxCol board (length (board!!0)) 'X')    -- Obtenim la columna que permet al contrari concatenar el màxim nombre de fitxes (m2).
    if m2 == 4 && m /= 4 then do
        let r = getRow board (c2-1)                         -- Si el contrari pot concatena 4 fitxes i nosaltres no podem, seleccionem aquella columna.
        return (c2,r)                                       
    else do
        let r = getRow board (c-1)                          -- En cas contrari, seleccionem la columna que hem obtingut.
        return (c,r)

moveAI board Smart diff = do
    let (c, score) = minimax board AI diff                  -- Obtenim la columna amb la màxima puntuació utilitzant l'algorisme minimax, amb profunditat diff.
    --print (c,score)                                         -- per debugar heuristica
    case score of
        (-1000) ->  moveAI board Greedy 0                   -- Si la partida la te guanyada el contrari, utilitzem greedy per si el contrincant es despista.
        _ ->
            case c of
                Just col -> do
                    let r = getRow board (col-1)
                    return (col,r)                              -- Retornem posició obtinguda
                _ -> putStrLn "err: NO SPACE" >> return (1,1)   -- Cas de error. No hauria de sortir en cap cas.



-- -- -- -- -- -- --
-- --  JUGADA  -- --
-- -- -- -- -- -- --


{- Retorna un tauler buit de la mida indicat -}
emptyBoard :: Int -> Int -> Board
emptyBoard n m =  replicate n $ replicate m ' '


{- Retorna un nou tauler amb la fitxa del jugador al forat (r,c) -}
changeBoard :: Board -> Player -> Int -> Int -> Board
changeBoard board player c r = 
    replace r (replace c simbol (board !! r)) board 
    where
        simbol = case player of
            AI -> 'O'
            P1 -> 'X'
            AI2 -> 'X'


{- Modifica l'element "n" de la llista -}
replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a:xs
replace n a (x:xs) = x:(replace (n-1) a xs)


{- Retorna la fila on caura la fitxa a la columna "c" -}
getRow :: Board -> Int -> Int
getRow board c = 
    length $ takeWhile (==' ') (foldr (\x acc -> x!!c : acc) [] board)


{- Comprova que la columna sigui valida -}
checkColumn :: Board -> Int -> Bool
checkColumn board c
    | c > length (board!!0) = False     -- intervals del tauler
    | c < 1 = False
    | otherwise = True


{- Comprova si el tauler esta ple -}
isFull :: Board -> Bool
isFull board = all (/=' ') (board!!0)   -- mira si la fila de sobre està plena


{- Pregunta i obté la columna que l'usuari esculli -}
askColumn :: Board -> [Char] -> IO (Int,Int)
askColumn board msg = do
    putStrLn $ showTurn $ board     -- mostrem el tauler actual
    putStr msg                      -- escrivim el missatge
    hFlush stdout
    input <- getLine
    let col = readMaybe input :: Maybe Int
    case col of
        Nothing -> putStrLn ("\t\x1b[31mERROR: Should be Integer\x1b[0m") >> askColumn board msg    -- si no és un enter, indiquem l'error i tornem a preguntar.
        Just c -> 
            if not $ checkColumn board c then do
                putStrLn ("\t\x1b[31mERROR: Invalid column \x1b[0m") >> askColumn board msg         -- si no és una columna valida, tornem a preguntar.
            else do
                let r = getRow board (c-1)
                if r == 0 then do
                    putStrLn ("\t\x1b[31mERROR: No empty holes in column \x1b[0m" ++ (show c)) >> askColumn board msg   -- si la columna està plena, tornem a preguntar.
                else
                    return (c,r)    -- retornem la columna i la fila de la següent fitxa.


{- Mostra missatge de si l'usuari ha guanyat o ha perdut -}
showWinner :: Board -> Char -> IO ()
showWinner board winner
    | winner == 'X' = putStrLn $ (showTurn board) ++ "\n\n\x1b[32m  Winner!!! Good Job \x1b[0m\n"-- ++ (show winner)
    | winner == 'O' = putStrLn $ (showTurn board) ++ "\n\n\x1b[31m  Game OVER! - You've lost! \x1b[0m\n"-- ++ (show winner)


{- Seguent jugada de la partida.-}
play :: Board -> Player -> Strategy -> Int -> IO ()
play board player str diff
    | winner /= '?' = showWinner board winner >> main                         -- Cas on un jugador ja ha guanya la partida
    | isFull board = putStrLn ((showTurn board) ++ "\n  It's a TIE") >> main  -- Cas on el tauler esta ple.

    | player == P1 = do
        (c,r) <- askColumn board "  Your turn\n  Choose column: "               -- Obtenim la posició de la següent fitxa
        let next_board = changeBoard board P1 (c-1) (r-1)                       -- Obtenim el tauler amb la nova fitxa
        play next_board AI str diff                                             -- Nova jugada (jugador AI)
        
    | otherwise = do                                                            -- Ídem. Mateix procediment que abans, ara amb player AI.
        putStrLn $ showTurn board ++ "\n  AI turn\n  thinking... "
        (c,r) <- moveAI board str diff                                          -- Obté la posició de la següent fitxa segons l'estratègia "str"
        putStrLn $ "\n  Column -> " ++ (show c)
        let next_board = changeBoard board AI (c-1) (r-1)
        play next_board P1 str diff                                             -- Nova jugada (jugador P1)
        --play next_board AI2 str diff
    where
        winner = checkWinner board



-- -- -- -- -- -- -- --
-- -- MAIN/DRIVER -- --
-- -- -- -- -- -- -- --


{- Pregunta el missatge "msg" i espera rebre un enter que compleixi la restricció "f" -}
askInt :: [Char] -> (Int -> Bool) -> IO Int
askInt msg f = do
    putStr msg
    hFlush stdout   -- escriu per pantalla tot el que estigui al buffer de sortida.
    input <- getLine
    let num = readMaybe input :: Maybe Int
    case num of
        Nothing -> putStrLn "\n\t\x1b[31mERROR: Should be Integer\x1b[0m" >> askInt msg f   -- cas que no sigui enter
        Just n -> 
            if not $ f n then do
                putStrLn ("\n\t\x1b[31mERROR: Invalid value\x1b[0m") >> askInt msg f    -- cas que no compleixi la restricció "f"
            else return n


{- Driver del programa. Comença una nova partida amb la mida que indica l'usuari i l'estratègia escollida.
Si l'estratègia és smart, també es pot escollir la dificultat, que represetarà la profunditat de l'algorisme minimax 
L'usuari (P1) serà el primer jugador de la partida.  -}
main :: IO ()
main = do 
    putStrLn "\n\t ¡New Game!\n\t————————————\n Board size:"

    n <- askInt "   Rows    = " (>0) 
    m <- askInt "   Columns = " (>0)

    putStrLn "\n AI Strategies:\n   1. random\n   2. greedy\n   3. smart"
    str <- askInt " Choose strategy: " (\x -> 1<=x && x<=3)
    case str of
        1 -> play (emptyBoard n m) P1 Random 0
        2 -> play (emptyBoard n m) P1 Greedy 0
        3 -> do
            diff <- askInt " Difficulty (recommended 4 or 5) = " (>0)
            play (emptyBoard n m) P1 Smart diff




