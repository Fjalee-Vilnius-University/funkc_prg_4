module Main where
import Data.List as L
import Data.Char as C
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)

---haskel executable
    ---executable parameters
---stdin
---stdout
---exit code



-------------------------------------------------------------------
------------------------Parser from LW2----------------------------
-------------------------------------------------------------------

data JsonLikeValue = JLString String | JLInt Int | JLMap [(String, JsonLikeValue)] | JLArray [JsonLikeValue] deriving (Show, Eq)
data InvalidState = Order | Duplicates deriving (Show, Eq)
type To = [[(Int, Char)]]


parse :: String -> JsonLikeValue
parse str = 
    if (isMessageMapEmpty str)
    then JLMap []
    else
        case parseJLMap str str of
            Left a -> error $ show a
            Right (b, "") -> b
            Right _ -> error "Some values are outside of map"

eitherParse :: String -> Either String JsonLikeValue
eitherParse str = 
    case etherIsMessageMapEmpty str of
        Left a -> Left a
        Right True -> Right $ JLMap []
        Right False ->
            case parseJLMap str str of
                Left _ -> Left "CantParse"
                Right (b, "") -> Right b
                Right _ -> Left "CantParse"

isMessageMapEmpty :: String -> Bool
isMessageMapEmpty str = 
    if (length str < 29)
    then 
        if (length str > 2)
        then error $ "Invalid message received: " ++ str ++" message has to contain ether empty map \"de\" or valid values inside the map"
        else True
    else False

etherIsMessageMapEmpty :: String -> Either String Bool
etherIsMessageMapEmpty str = 
    if (length str < 29)
    then 
        if (length str > 2)
        then Left "CantParse"
        else Right True
    else Right False

parseJLMap :: String -> String -> Either String (JsonLikeValue, String)
parseJLMap ('d':t) orgStr = 
    case parseAllMapedJLValues t orgStr of
        Left a -> Left a
        Right (mapBody, rest) -> Right (mapBody, rest)
parseJLMap dnStartD orgStr = Left ("Error around character " ++ show errPos ++ " received string: " ++ dnStartD ++ " map has to start with a 'd'")
    where 
        errPos = lenDiff orgStr dnStartD

lenDiff :: String -> String -> Int
lenDiff str1 str2 = (length str1) - (length str2)

parseAllMapedJLValues :: String -> String -> Either String (JsonLikeValue, String)
parseAllMapedJLValues ('e':t) _ = Right (JLMap [], t)
parseAllMapedJLValues str orgStr = 
    case parseMapedJLValue str orgStr of
        Left a -> Left a
        Right ((key, value), rest) -> 
            case parseAllMapedJLValues rest orgStr of
                Left a -> Left a
                Right (JLMap acc, rest1) -> Right $ (JLMap ([(key, value)] ++ acc), rest1)

parseMapedJLValue :: String -> String -> Either String ((String, JsonLikeValue), String)
parseMapedJLValue str orgStr = 
    case parseString str orgStr of
        Left a -> Left a
        Right (key, rest) ->
            case parseJLValue rest orgStr of
                Left a -> Left a
                Right (value, rest') -> Right ((key, value), rest')

parseString :: String -> String -> Either String (String, String)
parseString str orgStr =
    let
        errPos = lenDiff orgStr str
        strLen = if C.isDigit $ head str
            then L.takeWhile C.isDigit str
            else "not declared"
        postfix = L.drop (length strLen) str
    in
        case strLen of 
            "not declared" -> Left ("Error around character " ++ show errPos ++ " received string: " ++ str ++ " Length of the string was not declared")
            _ ->
                case postfix of
                (':':r) -> Right (L.take (read strLen) r, L.drop (read strLen) r)
                _ -> Left ("Error around character " ++ show errPos ++ " received string: " ++ str ++ " Invalid string")

parseJLValue :: String -> String -> Either String (JsonLikeValue, String)
parseJLValue ('d':t) orgStr = 
    case parseJLMap('d':t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
parseJLValue ('l':t) orgStr = 
    case parseJLArray [] ('l':t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
parseJLValue ('i':t) orgStr = 
    case parseJLInt ('i':t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
parseJLValue (h:t) orgStr = 
    let
        errPos = lenDiff orgStr (h:t)
    in
    if C.isDigit h
    then 
        case parseJLString (h:t) orgStr of
            Left a -> Left a
            Right (a, b) -> Right (a, b)
    else Left ("Error around character " ++ show errPos ++ " received string: " ++ (h:t) ++ " JsonLikeValue has to start with a 'd' or a 'l' or an 'i' or a digit")
parseJLValue [] orgStr = Left ("Error around character " ++ show errPos ++ ", Empty JLValue")
    where
        errPos = lenDiff orgStr []

parseJLArray :: [JsonLikeValue] -> String -> String -> Either String (JsonLikeValue, String)
parseJLArray [] ('l':t) orgStr =
    case parseJLValue t orgStr of
        Left a -> Left a
        Right (value, (fstCh : rest)) ->
            case fstCh of
                'e' -> Right (JLArray ([] ++ [value]), rest)
                _ -> parseJLArray ([] ++ [value]) (fstCh : rest) orgStr
parseJLArray parsedArrEls (h:t) orgStr =
    case parseJLValue (h:t) orgStr of
        Left a -> Left a
        Right (value, (fstCh : rest)) ->
            case fstCh of
                'e' -> Right (JLArray (parsedArrEls ++ [value]), rest)
                _ -> parseJLArray (parsedArrEls ++ [value]) (fstCh : rest) orgStr
parseJLArray [] [] orgStr = Left ("Error around character " ++ show errPos ++ "Empty Array")
    where
        errPos = lenDiff orgStr []
parseJLArray _ dnStartL orgStr = Left ("Error around character " ++ show errPos ++ " received string: " ++ dnStartL ++ " list has to start with an 'l'")
    where
        errPos = lenDiff orgStr dnStartL

parseJLInt :: String -> String -> Either String (JsonLikeValue, String)
parseJLInt ('i':t) orgStr = 
        let 
            errPos = lenDiff orgStr ('i':t)
        in
        case C.isDigit $ head t of
            False -> Left ("Error around character " ++ show errPos ++ ", Integer has 0 digits")
            True ->
                    let
                        prefix = L.takeWhile C.isDigit t
                        postfix = L.drop (length prefix) t
                    in
                        case postfix of
                            ('e':r) -> Right (JLInt (read prefix), r)
                            _ -> Left ("Error around character " ++ show errPos ++ " received string: " ++ ('i':t) ++ " Integer has to end with an 'e'")
parseJLInt [] orgStr = Left ("Error around character " ++ show errPos ++ ", Empty Int")
    where
        errPos = lenDiff orgStr []
parseJLInt dnStartI orgStr = Left ("Error around character " ++ show errPos ++ " received string: " ++ dnStartI ++ " Integer has to start with an 'i'")
    where
        errPos = lenDiff orgStr dnStartI

parseJLString :: String -> String -> Either String (JsonLikeValue, String)
parseJLString str orgStr =
    let
        errPos = lenDiff orgStr str
    in
        if C.isDigit $ head str
            then case L.drop (length (L.takeWhile C.isDigit str)) str of
                (':':r) -> Right (JLString $ L.take (read (L.takeWhile C.isDigit str)) r, L.drop (read (L.takeWhile C.isDigit str)) r)
                _ -> Left ("Error around character " ++ show errPos ++ ", Invalid string")
            else Left ("Error around character " ++ show errPos ++ ", Length of the string was not declared")

-------------------------------------------------------------------
-----------------------Converter from LW2--------------------------
-------------------------------------------------------------------

convert :: Int -> JsonLikeValue -> Either InvalidState To
convert size wholeMap =
    case getAllTurnsArr wholeMap ([], [], []) of
        Left a -> Left a
        Right allTurnsArr -> 
            case parseArrToLIL allTurnsArr (createEmptyLILArr size []) of
                Left a -> Left a
                Right a -> Right a
        
createEmptyLILArr :: Int -> [[(Int, Char)]] -> [[(Int, Char)]]
createEmptyLILArr 0 arr = arr
createEmptyLILArr size arr = createEmptyLILArr (size-1) (arr ++ [[]])

parseArrToLIL :: ([Int], [Int], [Char]) -> [[(Int, Char)]] -> Either InvalidState [[(Int, Char)]]
parseArrToLIL ([], [], []) lilArr = Right lilArr
parseArrToLIL (xsArr, ysArr, vsArr) lilArr = 
    let
        ythArr = lilArr !! head ysArr
        newMove = (head xsArr, head vsArr)
    in 
        case addMoveToArr ythArr newMove of
            Left a -> Left a
            Right a -> parseArrToLIL (tail xsArr, tail ysArr, tail vsArr) (replace a (head ysArr) lilArr)


addMoveToArr :: [(Int, Char)] -> (Int, Char) -> Either InvalidState [(Int, Char)]
addMoveToArr allMovesArr (x, v) = 
    case findPosToInsert allMovesArr x 0 of
        Left a -> Left a
        Right pos -> Right $ insertAt (x,v) pos allMovesArr

insertAt :: a -> Int -> [a] -> [a]
insertAt newEl _ [] = [newEl]
insertAt newEl 0 arr = (newEl:arr)
insertAt newEl i (a:arr) = (a : insertAt newEl (i - 1) arr)

findPosToInsert :: [(Int, Char)] -> Int -> Int -> Either InvalidState Int
findPosToInsert [] _ _ = Right 0
findPosToInsert arr cord ptr = 
    if ((length arr) - 1 < ptr)
    then Right (ptr)
    else
        if (fst (arr !! ptr)) == cord
        then Left Duplicates
        else
            if (fst (arr !! ptr)) > cord
            then Right (ptr)
            else 
                case findPosToInsert arr cord (ptr+1) of
                    Left a -> Left a
                    Right a -> Right a

getAllTurnsArr :: JsonLikeValue -> ([Int], [Int], [Char]) -> Either InvalidState ([Int], [Int], [Char])
getAllTurnsArr wholeMap allTurnsArr = 
    case mapFind wholeMap "last" of
        Just lstLast -> 
            let
                allTurnsArr' = addTurn lstLast allTurnsArr
            in
                case isCorrOrder allTurnsArr' of 
                    False -> Left Order
                    True ->
                        case delFromMap wholeMap ("last", lstLast) of
                            JLMap (h:_) -> getAllTurnsArr (snd h) allTurnsArr'
                            JLMap [] -> Right allTurnsArr'
        Nothing -> Right ([], [], [])

mapFind :: JsonLikeValue -> String -> Maybe JsonLikeValue 
mapFind (JLMap []) _ = Nothing
mapFind (JLMap (h:t)) needed = 
    if (fst h) == needed
    then Just $ snd h
    else mapFind (JLMap t) needed

addTurn :: JsonLikeValue -> ([Int],[Int],[Char]) -> ([Int],[Int],[Char])
addTurn turn (xsArr,ysArr,vsArr) = 
    case turn of
        JLArray [] -> (xsArr,ysArr,vsArr)
        JLArray (JLMap (turnTuple) : []) -> 
                    let
                        turnArr = snd $ head turnTuple
                        xsParsedInt = parseJLIntToInt $ (parseJLArrayToArray turnArr) !! 0
                        ysParsedInt = parseJLIntToInt $ (parseJLArrayToArray turnArr) !! 1
                        vsParsedStr = parseJLStringToString $ (parseJLArrayToArray turnArr) !! 2
                    in
                        (xsArr ++ [xsParsedInt], ysArr ++ [ysParsedInt], vsArr ++ vsParsedStr)
        a -> error $ "Turn has to be in an array, but received " ++  show a

parseJLIntToInt :: JsonLikeValue -> Int
parseJLIntToInt theInt =
    case theInt of
        JLInt a -> a

parseJLStringToString :: JsonLikeValue -> String
parseJLStringToString str =
    case str of
        JLString a -> a
        a -> error $ show a

parseJLArrayToArray :: JsonLikeValue -> [JsonLikeValue]
parseJLArrayToArray arr =
    case arr of
        JLArray a -> a

isCorrOrder :: ([Int], [Int], [Char]) -> Bool
isCorrOrder (_, _, []) = True
isCorrOrder (_, _, (_:[])) = True
isCorrOrder (_, _, vsArr) = 
    if head (reverse vsArr) == head (drop 1 (reverse vsArr))
    then False
    else True

delFromMap :: JsonLikeValue -> (String, JsonLikeValue) -> JsonLikeValue
delFromMap wholeMap itemDel = 
    case wholeMap of
        JLMap arrayOfTuples -> JLMap $ delete itemDel arrayOfTuples

-------------------------------------------------------------------
--------------Convert board to message for another bot-------------
-------------------------------------------------------------------

convertBack :: ([Int], [Int], [Char]) -> String
convertBack mTuple = 
    let
        moves = fromXYVTuplesToXYVArrays mTuple []
    in
        xyvArrayToJson moves

xyvArrayToJson :: [(Int, Int, Char)] -> String
xyvArrayToJson [] = ""
xyvArrayToJson (h:t) =
    let
        moveJsonStr = xyvTupleToLMDL h
    in
        case xyvArrayToJson t of
        "" -> jsonMap[("last", moveJsonStr)]
        a -> jsonMap[("prev", a), ("last", moveJsonStr)]

xyvTupleToLMDL :: (Int, Int, Char) -> String
xyvTupleToLMDL (x, y, v) = jsonList [jsonMap [("data", (jsonList [jsonInt x, jsonInt y, jsonString [v]]))]]

fromXYVTuplesToXYVArrays :: ([Int], [Int], [Char]) -> [(Int, Int, Char)] -> [(Int, Int, Char)]
fromXYVTuplesToXYVArrays ([], [], []) acc = acc
fromXYVTuplesToXYVArrays ((hx:tx), (hy:ty), (hv:tv)) acc =
    let
        move = (hx, hy, hv)
    in
        fromXYVTuplesToXYVArrays (tx, ty, tv) (acc ++ [move])

jsonMap :: [(String, String)] -> String
jsonMap arr = jsonMap' arr []


jsonMap' :: [(String, String)] -> String -> String
jsonMap' [] acc = "d" ++ acc ++ "e"
jsonMap' (h:t) acc = 
    let
        key = fst h
        value = snd h
        keyLen = show (length key)
        newItem = keyLen ++ ":" ++ key ++ value
    in
        jsonMap' t (acc ++ newItem)

jsonList :: [String] -> String
jsonList arr = jsonList' arr []

jsonList' :: [String] -> String -> String
jsonList' [] acc = "l" ++ acc ++ "e"
jsonList' (h:t) acc = jsonList' t (acc ++ h)

jsonInt :: Int -> String
jsonInt a = "i" ++ show a ++ "e"

jsonString :: String -> String
jsonString a = show (length a) ++ ":" ++ a

-------------------MAIN's-----------------------

main :: IO()
main = do
    args <- getArgs
    msg <- getLine
    let 
        (myStdOut, myErrOut, myExitCode) = if (msg == "*") then getOutput "*" else getOutput msg in
        case myExitCode of
            100 -> do exitWith $ ExitFailure myExitCode
            101 -> do exitWith $ ExitFailure myExitCode
            _ -> do
                putStrLn myStdOut
                hPutStrLn stderr myErrOut
                case myExitCode of
                    0 -> exitWith ExitSuccess
                    _ -> exitWith $ ExitFailure myExitCode

getOutput :: String -> (String, String, Int)
getOutput jsonMsg = 
    case eitherParseToLilBoard jsonMsg of
        Left "CantParse" ->
            let
                myStdOut = jsonMsg
                myErrOut = getStrToPrintStatusMsg ([[]], "Incoming message is malformed (bad syntax)")
                myExitCode = 100
            in
                (myStdOut, myErrOut, myExitCode)
        Left "Order" -> 
            let
                myStdOut = jsonMsg
                myErrOut = getStrToPrintStatusMsg ([[]], "Incoming message is semanticallly invalid (e.g. 2 moves to a same cell or game is already ended)")
                myExitCode = 101
            in
                (myStdOut, myErrOut, myExitCode)
        Left "Duplicates" ->
            let
                myStdOut = jsonMsg
                myErrOut = getStrToPrintStatusMsg ([[]], "Incoming message is semanticallly invalid (e.g. 2 moves to a same cell or game is already ended)")
                myExitCode = 101
            in
                (myStdOut, myErrOut, myExitCode)
        Right a ->
            let
                board = populateBlankVals a
            in
                if (isWin board /= 'b' || isBoardFull board) then
                    let
                        myStdOut = jsonMsg
                        myErrOut = getStrToPrintStatusMsg (board, "I cannot perform any moves because game is already ended (board is full or there is a winner)")
                        myExitCode = 20
                    in
                        (myStdOut, myErrOut, myExitCode)
                else
                    case maybeTakeTurnRetLil' board of
                        Nothing ->
                            let
                                myStdOut = jsonMsg
                                myErrOut = getStrToPrintStatusMsg (board, "I cannot perform any moves because game is already ended (board is full or there is a winner)")
                                myExitCode = 20
                            in
                                (myStdOut, myErrOut, myExitCode)
                        Just boardAfterMyTurn ->
                            let
                                myStdOut = takeTurnIfPossibleRetJsonMessage' jsonMsg -- not using boardAfterMyTurn because it doesnt show order of the moves to convert to jsonMsg
                                (myMoveX, myMoveY, myMoveV) = findDif board boardAfterMyTurn
                                myErrOut = getStrToPrintStatusMsg (boardAfterMyTurn, ("My Turn is " ++ (show myMoveV) ++ " to " ++ "(" ++ (show myMoveX) ++ "," ++ (show myMoveY) ++ ")"))
                                myExitCode 
                                    | (isWin boardAfterMyTurn /= 'b') = 10
                                    | (isBoardFull boardAfterMyTurn) = 12
                                    | otherwise = 0
                            in
                                (myStdOut, myErrOut, myExitCode)


------------------------------------------------------------
------------------------For getOutput-----------------------
------------------------------------------------------------

parseToLilBoard :: String -> To
parseToLilBoard str = 
    case convert 3 (parse str) of
        Left a -> error $ show a
        Right a -> a

eitherParseToLilBoard :: String -> Either String To
eitherParseToLilBoard str = 
    case eitherParse str of
        Left a -> Left a
        Right a -> 
            case convert 3 a of
                Left b -> Left $ show b
                Right b -> Right b

---------

maybeTakeTurnRetLil' :: To -> Maybe To
maybeTakeTurnRetLil' board = 
    case miniMax board of
        ([], _) -> Nothing
        (a, _) -> Just a
        
-------------------------minimax--------------------------

boardOneGenScores :: To -> ([To], [Int])
boardOneGenScores board =
    let
        allPossMoves = genAllPossibleMoves board board []
        allPossScores = calcAllBoardsScore allPossMoves []
    in
        (allPossMoves, allPossScores)

pickBoardWithScore :: [To] -> [Int] -> Int -> Maybe To
pickBoardWithScore boards scores player =
    case findIndex (== player) scores of
        Nothing -> Nothing
        Just i -> Just $ boards !! i

miniMax :: To -> (To,Int)
miniMax board =
    if isBoardFull board then
        ([], 0)
    else
        let
            player
                | isXTurn board = ('X', 10)
                | otherwise = ('O', -10)
            (fstGenBoards, fstGenScores) = boardOneGenScores board
        in
            case length fstGenBoards of
                1 -> (head fstGenBoards, head fstGenScores)
                9 -> (fstGenBoards !! 4, 0)
                _ ->
                    case pickBoardWithScore fstGenBoards fstGenScores (snd player) of
                        Just i -> (i, snd player)
                        Nothing -> 
                            ---Paima lenta 1st gen kurioje laimi, kitu atjevu Nothing
                            let
                                boardsNScores = map miniMax fstGenBoards
                                (futBoards, futScores) = sepBoardsFromScores boardsNScores ([], [])
                            in
                                    ----------THIS LINE IS FOR DEBUGING WHEN BOARD IS INPUTED---------------------------
                                --case board of
                                    --[[(0, 'O'), (1, 'b'), (2, 'X')], [(0, 'b'), (1, 'b'), (2, 'X')], [(0, 'b'), (1, 'b'), (2, 'b')]] -> error $ show (futBoards, futScores)
                                    ------------------------------------------------------------------------------------
                                    case pickBoardWithScore fstGenBoards futScores (snd player) of
                                        Just i -> (i, snd player)
                                        Nothing -> 
                                            case pickBoardWithScore fstGenBoards futScores 0 of
                                                Just i -> (i, 0)
                                                Nothing -> (head fstGenBoards, -(snd player))

-------------------------------------------------------------------
------------------------           --------------------------------
-------------------------------------------------------------------

whichSqBlank :: To -> Maybe (Int, Int) 
whichSqBlank ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : [])  : []) 
    | (snd sq1 == 'b') = Just (0, 0)
    | (snd sq2 == 'b') = Just (0, 1)
    | (snd sq3 == 'b') = Just (0, 2)
    | (snd sq4 == 'b') = Just (1, 0)
    | (snd sq5 == 'b') = Just (1, 1)
    | (snd sq6 == 'b') = Just (1, 2)
    | (snd sq7 == 'b') = Just (2, 0)
    | (snd sq8 == 'b') = Just (2, 1)
    | (snd sq9 == 'b') = Just (2, 2)
    | otherwise = Nothing
------------------------------------------------------------

genAllPossibleMoves :: To -> To -> [To] -> [To]
genAllPossibleMoves board genBoard acc = 
    case genPossibleMove board genBoard of
            Left _ -> acc
            Right (newMove, newGenBoard) -> genAllPossibleMoves board newGenBoard (acc ++ [newMove])

genPossibleMove :: To -> To -> Either String (To, To)
genPossibleMove orgBoard board = 
    case whichSqBlank board of
        Nothing -> Left "No more possible moves"
        Just (row, col) -> 
            let 
                player
                    | isXTurn orgBoard = 'X'
                    | otherwise = 'O'
                newRowAfterMove = replace (col, player) col (orgBoard !! row)
                newBoardAfterMove = replace newRowAfterMove row orgBoard
                newRowForBoard = replace (col, player) col (board !! row)
                newBoard = replace newRowForBoard row board
            in
                Right (newBoardAfterMove, newBoard)

---------

takeTurnIfPossibleRetJsonMessage' :: String -> String
takeTurnIfPossibleRetJsonMessage' msg = 
    case maybeTakeTurnRetTurnOrder' msg of
        Nothing -> msg
        Just a -> convertBack a

maybeTakeTurnRetTurnOrder' :: String -> Maybe ([Int], [Int], [Char])
maybeTakeTurnRetTurnOrder' msg = 
    let
        oldBoard = populateBlankVals $ parseToLilBoard msg
    in 
        case (maybeTakeTurnRetLil' oldBoard) of
            Nothing -> Nothing
            Just newBoard ->
                case (maybeFindDif oldBoard newBoard) of
                    Nothing -> Nothing
                    Just newMove -> 
                        case getAllTurnsArr (parse msg) ([], [], []) of
                            Left _ -> Nothing
                            Right movesOrder -> Just $ addMoveToOrderedMoves movesOrder newMove

----------

addMoveToOrderedMoves :: ([Int], [Int], [Char]) -> (Int, Int, Char) -> ([Int], [Int], [Char])
addMoveToOrderedMoves (xs, ys, vs) (x, y, v) = ([x] ++ xs, [y] ++ ys, [v] ++ vs)
























----------------------------------------------
--------Simply understandable functions-------
----------------------------------------------
isXTurn :: To -> Bool
isXTurn ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : [])  : []) =
    let
        allTurnsValues = [snd sq1, snd sq2, snd sq3, snd sq4, snd sq5, snd sq6, snd sq7, snd sq8, snd sq9]
        xNmTurns = calcPlayerTurns allTurnsValues 'X' 0
        oNmTurns = calcPlayerTurns allTurnsValues 'O' 0
    in
        if (oNmTurns < xNmTurns)
        then False
        else True
isXTurn a = error $ "Cant check if X turn: Invalid board" ++ show a

calcPlayerTurns :: [Char] -> Char -> Int -> Int
calcPlayerTurns [] _ acc = acc
calcPlayerTurns (h:t) player acc
    | h == player = calcPlayerTurns t player (acc + 1)
    | otherwise = calcPlayerTurns t player acc
    
replace :: a -> Int -> [a] -> [a]
replace newEl 0 arr = newEl : (tail arr)
replace newEl i (a:arr) = (a : replace newEl (i-1) arr)

isBoardFull :: To -> Bool
isBoardFull ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : []) : [])
    | (snd sq1 /= 'b' && snd sq2 /= 'b' && snd sq3 /= 'b' && snd sq4 /= 'b' && snd sq5 /= 'b' &&
       snd sq6 /= 'b' && snd sq7 /= 'b' && snd sq8 /= 'b' && snd sq9 /= 'b') = True
    | otherwise = False
isBoardFull b = error $ "Cannot check if board is full: Invalid board " ++ show b
    
isWin :: To -> Char
isWin ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : [])  : []) 
    --Diagonals
    | (snd sq1 == snd sq5 && snd sq5 == snd sq9 && snd sq9 /= 'b') = snd sq9
    | (snd sq3 == snd sq5 && snd sq5 == snd sq7 && snd sq7 /= 'b') = snd sq7
    --Horizontals
    | (snd sq1 == snd sq2 && snd sq2 == snd sq3 && snd sq3 /= 'b') = snd sq3
    | (snd sq4 == snd sq5 && snd sq5 == snd sq6 && snd sq6 /= 'b') = snd sq6
    | (snd sq7 == snd sq8 && snd sq8 == snd sq9 && snd sq9 /= 'b') = snd sq9
    --Verticals
    | (snd sq1 == snd sq4 && snd sq4 == snd sq7 && snd sq7 /= 'b') = snd sq7
    | (snd sq2 == snd sq5 && snd sq5 == snd sq8 && snd sq8 /= 'b') = snd sq8
    | (snd sq3 == snd sq6 && snd sq6 == snd sq9 && snd sq9 /= 'b') = snd sq9
    | otherwise = 'b'
isWin b = error $ "Cannot check if player won: Invalid board " ++ show b

replaceAllBWithSpace :: To -> To
replaceAllBWithSpace ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : [])  : []) = 
    let
        sq1' = (fst sq1 ,ifBSpace $ snd sq1)
        sq2' = (fst sq2 ,ifBSpace $ snd sq2)
        sq3' = (fst sq3 ,ifBSpace $ snd sq3)
        sq4' = (fst sq4 ,ifBSpace $ snd sq4)
        sq5' = (fst sq5 ,ifBSpace $ snd sq5)
        sq6' = (fst sq6 ,ifBSpace $ snd sq6)
        sq7' = (fst sq7 ,ifBSpace $ snd sq7)
        sq8' = (fst sq8 ,ifBSpace $ snd sq8)
        sq9' = (fst sq9 ,ifBSpace $ snd sq9)
    in
        [[sq1', sq2', sq3'], [sq4', sq5', sq6'], [sq7', sq8', sq9']]

takeNonB :: Char -> Char -> Char
takeNonB 'b' 'b' = error "Both b chars"
takeNonB 'b' w = w
takeNonB w 'b' = w
takeNonB _ _ = error "Both non b chars"

findDif :: [[(Int, Char)]] -> [[(Int, Char)]] -> (Int, Int, Char)
findDif ((sq1a : sq2a : sq3a : []) : (sq4a : sq5a : sq6a : [])  : (sq7a : sq8a : sq9a : [])  : [])
        ((sq1b : sq2b : sq3b : []) : (sq4b : sq5b : sq6b : [])  : (sq7b : sq8b : sq9b : [])  : []) 
    | (sq1a /= sq1b) = (0, 0, takeNonB (snd sq1a) (snd sq1b))
    | (sq2a /= sq2b) = (1, 0, takeNonB (snd sq2a) (snd sq2b))
    | (sq3a /= sq3b) = (2, 0, takeNonB (snd sq3a) (snd sq3b))
    | (sq4a /= sq4b) = (0, 1, takeNonB (snd sq4a) (snd sq4b))
    | (sq5a /= sq5b) = (1, 1, takeNonB (snd sq5a) (snd sq5b))
    | (sq6a /= sq6b) = (2, 1, takeNonB (snd sq6a) (snd sq6b))
    | (sq7a /= sq7b) = (0, 2, takeNonB (snd sq7a) (snd sq7b))
    | (sq8a /= sq8b) = (1, 2, takeNonB (snd sq8a) (snd sq8b))
    | (sq9a /= sq9b) = (2, 2, takeNonB (snd sq9a) (snd sq9b))
    | otherwise = error "Two boards are the same"

maybeFindDif :: [[(Int, Char)]] -> [[(Int, Char)]] -> Maybe (Int, Int, Char)
maybeFindDif ((sq1a : sq2a : sq3a : []) : (sq4a : sq5a : sq6a : [])  : (sq7a : sq8a : sq9a : [])  : [])
        ((sq1b : sq2b : sq3b : []) : (sq4b : sq5b : sq6b : [])  : (sq7b : sq8b : sq9b : [])  : []) 
    | (sq1a /= sq1b) = Just (0, 0, takeNonB (snd sq1a) (snd sq1b))
    | (sq2a /= sq2b) = Just (1, 0, takeNonB (snd sq2a) (snd sq2b))
    | (sq3a /= sq3b) = Just (2, 0, takeNonB (snd sq3a) (snd sq3b))
    | (sq4a /= sq4b) = Just (0, 1, takeNonB (snd sq4a) (snd sq4b))
    | (sq5a /= sq5b) = Just (1, 1, takeNonB (snd sq5a) (snd sq5b))
    | (sq6a /= sq6b) = Just (2, 1, takeNonB (snd sq6a) (snd sq6b))
    | (sq7a /= sq7b) = Just (0, 2, takeNonB (snd sq7a) (snd sq7b))
    | (sq8a /= sq8b) = Just (1, 2, takeNonB (snd sq8a) (snd sq8b))
    | (sq9a /= sq9b) = Just (2, 2, takeNonB (snd sq9a) (snd sq9b))
    | otherwise = Nothing

populateBlankVals :: To -> To
populateBlankVals (row1 : row2 : row3 : []) 
    | (length row1 /= 3) = populateBlankVals ((addLowestBlank row1 0) : row2 : row3 : [])
    | (length row2 /= 3) = populateBlankVals (row1 : (addLowestBlank row2 0) : row3 : [])
    | (length row3 /= 3) = populateBlankVals (row1 : row2 : (addLowestBlank row3 0) : [])
    | otherwise = (row1 : row2 : row3 : []) 
populateDummyVals :: Show a1 => a1 -> a2
populateDummyVals b = error $ "Cannot populate board with blank values: Invalid board " ++ show b

addLowestBlank :: [(Int, Char)] -> Int -> [(Int, Char)]
addLowestBlank row acc  
    | (length row < acc+1) = insertAt (acc, 'b') acc row
    | (fst (row !! acc) == acc) = addLowestBlank row (acc+1)
    | otherwise = insertAt (acc, 'b') acc row
    
ifBSpace :: Char -> Char
ifBSpace ch
    |(ch == 'b') = ' '
    | otherwise = ch

sepBoardsFromScores ::  [(To, Int)] -> ([To], [Int]) -> ([To], [Int])
sepBoardsFromScores [] acc = acc
sepBoardsFromScores (h:t) (boards, scores) = sepBoardsFromScores t (boards ++ [fst h], scores ++ [snd h])

getStrToPrintStatusMsg :: (To, String) -> String
getStrToPrintStatusMsg (board, msg) = unlines $ [msg, getStrToDrawBoard board]

getStrToDrawBoard :: To -> String
getStrToDrawBoard board =
    let
        board' = replaceAllBWithSpace board
        ((sq1 : sq2 :sq3 : []) : (sq4 : sq5 :sq6 : [])  : (sq7 : sq8 :sq9 : [])  : []) = board'
    in
        unlines $ [[snd sq1] ++  " | " ++  [snd sq2] ++ " | " ++ [snd sq3] ,
                   "---------",
                   [snd sq4] ++  " | " ++  [snd sq5] ++ " | " ++ [snd sq6] ,
                   "---------",
                   [snd sq7] ++  " | " ++  [snd sq8] ++ " | " ++ [snd sq9]]
  

-------------------------------------------------------
---------------------TEST------------------------------
-------------------------------------------------------

see1 :: To -> IO ()
see1 board = strToIO $ boardStr board

seeM :: [To] -> IO ()
seeM boardArr = 
    let
        boardArrStr = map boardStr boardArr
    in
        strArrToIO $ boardArrStr    

strToIO :: String -> IO()
strToIO str = putStr str

strArrToIO :: [String] -> IO()
strArrToIO str = putStr $ unlines str

boardStr :: To -> String
boardStr board = getStrToPrintStatusMsg (board, "")

calcAllBoardsScore' :: [To] -> [Int]
calcAllBoardsScore' [] = error "empty array shouldnt be passed into calcAllBoardsScore'"
calcAllBoardsScore' arr = calcAllBoardsScore arr []

calcAllBoardsScore :: [To] -> [Int] -> [Int]
calcAllBoardsScore [] acc = acc
calcAllBoardsScore (h:t) acc = calcAllBoardsScore t (acc ++ [calcScore h])

calcScore :: To -> Int
calcScore board = 
    let
        board' = populateBlankVals board
    in
        case isWin board' of
            'X' -> 10
            'O' -> -10
            'b' 
               | (isBoardFull board') -> 0
               | otherwise -> 50


------------------------------------------------------

-- main :: IO()
-- main = do
--     args <- getArgs
--     --let 
--         --args = ["O"] 
--     --in
--     case head args of
--         "X" -> do
--             msg <- getLine
--             let 
--                 (myStdOut, myErrOut, myExitCode) = if (msg == "*") then getOutput "de" else getOutput msg in
--                 case myExitCode of
--                     100 -> do exitWith $ ExitFailure myExitCode
--                     101 -> do exitWith $ ExitFailure myExitCode
--                     _ -> do
--                         putStrLn myStdOut
--                         hPutStrLn stderr myErrOut
--                         case myExitCode of
--                             0 -> exitWith ExitSuccess
--                             _ -> exitWith $ ExitFailure myExitCode
--         "O" -> do
--             msg <- getLine
--             let
--                 (myStdOut, myErrOut, myExitCode) = getOutput msg in 
--                 case myExitCode of
--                     100 -> do exitWith $ ExitFailure myExitCode
--                     101 -> do exitWith $ ExitFailure myExitCode
--                     _ -> do
--                         putStrLn myStdOut
--                         hPutStrLn stderr myErrOut
--                         case myExitCode of
--                             0 -> exitWith ExitSuccess
--                             _ -> exitWith $ ExitFailure myExitCode
                        
