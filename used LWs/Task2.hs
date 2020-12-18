module Task2 where
import Data.List as L
import Data.Char as C
import Task2Message

parseTest :: Bool
parseTest = 
    case (parse size message, expectedParse) of
        (Right m, Right e) -> m == e
        (Left _, Left _) -> True
        _ -> False
convertTest :: Bool
convertTest = convert size (either error id (parse size message')) == expectedConvert

--case (parse size message, expectedParse) of (Right m, Right e) -> m == e (Left _, Left _) -> True _ -> False
parse :: Int -> String -> Either String JsonLikeValue
parse _ str = 
    case parseJLMap str str of
        Left a -> Left a
        Right (b, "") -> Right b
        Right (b, _) -> Left "Some values are outside of map"

parseJLMap :: String -> String -> Either String (JsonLikeValue, String)
parseJLMap ('d':t) orgStr = 
    case parseAllMapedJLValues t orgStr of
        Left a -> Left a
        Right (mapBody, rest) -> Right (mapBody, rest)
parseJLMap dnStartD orgStr = Left ("Error around character " ++ show errPos ++ ", map has to start with a 'd'")
    where 
        errPos = lenDiff orgStr dnStartD

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

parseJLValue :: String -> String -> Either String (JsonLikeValue, String)
parseJLValue ('d':t) orgStr =
    case parseJLMap('d':t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
parseJLValue ('l':t) orgStr = 
    case parseJLArray ('l':t) orgStr of
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
    if (C.isDigit h)
    then 
        case parseJLString (h:t) orgStr of
            Left a -> Left a
            Right (a, b) -> Right (a, b)
    else Left ("Error around character " ++ show errPos ++ ", JsonLikeValue has to start with a 'd' or a 'l' or an 'i' or a digit")
parseJLValue [] orgStr = Left ("Error around character " ++ show errPos ++ ", Empty JLValue")
    where
        errPos = lenDiff orgStr []

parseJLArray :: String -> String -> Either String (JsonLikeValue, String)
parseJLArray ('l':t) orgStr =
    let
        errPos = lenDiff orgStr ('l':t)
    in
    case parseJLIntOrString t orgStr of
        Left a -> Left a
        Right (value, (fstCh : rest)) ->
            case fstCh of
                'e' -> Right (JLArray [value], rest)
                _ -> Left ("Error around character " ++ show errPos ++ ", list has to end with an 'e'")
parseJLArray [] orgStr = Left ("Error around character " ++ show errPos ++ "Empty Array")
    where
        errPos = lenDiff orgStr []
parseJLArray dnStartL orgStr = Left ("Error around character " ++ show errPos ++ ", list has to start with an 'l'")
    where
        errPos = lenDiff orgStr dnStartL

parseJLIntOrString :: String -> String -> Either String (JsonLikeValue, String)
parseJLIntOrString ('i':t) orgStr = 
    case parseJLInt ('i':t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
parseJLIntOrString (h:t) orgStr =
    let
        errPos = lenDiff orgStr (h:t)
    in
    if C.isDigit h
    then case parseJLString (h:t) orgStr of
        Left a -> Left a
        Right (a, b) -> Right (a, b)
    else Left ("Error around character " ++ show errPos ++ ", Value is nether an Int or a String")
parseJLIntOrString [] orgStr = Left ("Error around character " ++ show errPos ++ ", Empty Int or String")
    where
        errPos = lenDiff orgStr []


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
                            _ -> Left ("Error around character " ++ show errPos ++ ", Integer has to end with an 'e'")
parseJLInt [] orgStr = Left ("Error around character " ++ show errPos ++ ", Empty Int")
    where
        errPos = lenDiff orgStr []
parseJLInt dnStartI orgStr = Left ("Error around character " ++ show errPos ++ ", Integer has to start with an 'i'")
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
                
-- parseJLString :: String -> String -> Either String (JsonLikeValue, String)
-- parseJLString str orgStr =
--     let
--         errPos = lenDiff orgStr str
        
--         strLen = if C.isDigit $ head str
--             then L.takeWhile C.isDigit str
--             else "not declared"
--         postfix = L.drop (length strLen) str
--     in
--         case strLen of 
--             "not declared" -> Left ("Error around character " ++ show errPos ++ ", Length of the string was not declared")
--             _ ->
--                 case postfix of
--                 (':':r) -> Right (JLString $ L.take (read strLen) r, L.drop (read strLen) r)
--                 _ -> Left ("Error around character " ++ show errPos ++ ", Invalid string")

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
            "not declared" -> Left ("Error around character " ++ show errPos ++ ", Length of the string was not declared")
            _ ->
                case postfix of
                (':':r) -> Right (L.take (read strLen) r, L.drop (read strLen) r)
                _ -> Left ("Error around character " ++ show errPos ++ ", Invalid string")

lenDiff :: String -> String -> Int
lenDiff str1 str2 = (length str1) - (length str2)


--------------------------------------------------------------------------------------------------------------------------------


--convert size (either error id (parse size message')) == expectedConvert
convert :: Int -> JsonLikeValue -> Either InvalidState To
convert size wholeMap =
    case getallTurnsArr wholeMap ([], [], []) of
        Left a -> Left a
        Right allTurnsArr -> parseArrToLIL allTurnsArr (createEmptyLILArr size [])

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

insertAt :: a -> Int -> [a] -> [a]
insertAt newEl _ [] = [newEl]
insertAt newEl 0 arr = (newEl:arr)
insertAt newEl i (a:arr) = (a : insertAt newEl (i - 1) arr)

replace :: a -> Int -> [a] -> [a]
replace newEl 0 arr = newEl: (tail arr)
replace newEl i (a:arr) = (a : replace newEl (i-1) arr)

createEmptyLILArr :: Int -> [[(Int, Char)]] -> [[(Int, Char)]]
createEmptyLILArr 0 arr = arr
createEmptyLILArr size arr = createEmptyLILArr (size-1) (arr ++ [[]])

getallTurnsArr :: JsonLikeValue -> ([Int], [Int], [Char]) -> Either InvalidState ([Int], [Int], [Char])
getallTurnsArr wholeMap allTurnsArr = 
    case mapFind wholeMap "last" of
        Just lstLast -> 
            let
                allTurnsArr' = addTurn lstLast allTurnsArr
            in
                case isCorrOrder allTurnsArr' of 
                    False -> Left Order
                    True ->
                        case delFromMap wholeMap ("last", lstLast) of
                            JLMap (h:_) -> getallTurnsArr (snd h) allTurnsArr'
                            JLMap [] -> Right allTurnsArr'

isCorrOrder :: ([Int], [Int], [Char]) -> Bool
isCorrOrder (_, _, []) = True
isCorrOrder (_, _, (_:[])) = True
isCorrOrder (_, _, vsArr) = 
    if head (reverse vsArr) == head (drop 1 (reverse vsArr))
    then False
    else True

addTurn :: JsonLikeValue -> ([Int],[Int],[Char]) -> ([Int],[Int],[Char])
addTurn turn (xsArr,ysArr,vsArr) = 
    case turn of
        JLMap [] -> (xsArr,ysArr,vsArr)
        JLMap turnArr -> 
            case fst (head turnArr) of
                "xs" -> 
                    let
                        parsedInt = parseJLArrayToInt $ snd $ head turnArr
                    in
                        addTurn (JLMap (tail turnArr)) (xsArr ++ [parsedInt], ysArr, vsArr)
                "ys" -> 
                    let
                        parsedInt = parseJLArrayToInt $ snd $ head turnArr
                    in
                        addTurn (JLMap (tail turnArr)) (xsArr, ysArr ++ [parsedInt], vsArr)
                "vs" -> 
                    let
                        parsedStr = parseJLArrayToString $ snd $ head turnArr
                    in
                        addTurn (JLMap (tail turnArr)) (xsArr, ysArr, vsArr ++ parsedStr)

parseJLArrayToInt :: JsonLikeValue -> Int
parseJLArrayToInt arr =
    case arr of
        JLArray [JLInt a] -> a

parseJLArrayToString :: JsonLikeValue -> String
parseJLArrayToString arr =
    case arr of
        JLArray [JLString a] -> a

delFromMap :: JsonLikeValue -> (String, JsonLikeValue) -> JsonLikeValue
delFromMap wholeMap itemDel = 
    case wholeMap of
        JLMap arrayOfTuples -> JLMap $ delete itemDel arrayOfTuples

mapFind :: JsonLikeValue -> String -> Maybe JsonLikeValue 
mapFind (JLMap []) _ = Nothing
mapFind (JLMap (h:t)) needed = 
    if (fst h) == needed
    then Just $ snd h
    else mapFind (JLMap t) needed

mapFindLast :: JsonLikeValue -> String -> Maybe JsonLikeValue 
mapFindLast (JLMap []) _ = Nothing
mapFindLast (JLMap (h:t)) needed = 
    case mapFind (JLMap (h:t)) needed of
        Nothing -> Nothing
        Just value ->
            case isMapContains value needed of
                False -> Just value
                True -> mapFindLast value needed
            
isMapContains ::  JsonLikeValue -> String -> Bool
isMapContains (JLMap (h:t)) needed = 
    case mapFind (JLMap (h:t)) needed of
        Nothing -> False
        Just _ -> True