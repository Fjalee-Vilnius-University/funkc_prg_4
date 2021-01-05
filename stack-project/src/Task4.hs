module Task4 where
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import System.Environment
import Control.Applicative
import Control.Monad
import Data.Monoid

import Data.List
import Data.Char

type Parser a = ExceptT String (State String) a

data JsonLikeValue =
    JLString String |
    JLInt Int |
    JLMap [(String, JsonLikeValue)] |
    JLArray [JsonLikeValue]
    deriving (Show, Eq)

------------------------------------------------------------------
------------------------------ Parsing ---------------------------
------------------------------------------------------------------

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)
    
parseJLMessage :: String -> (Either String JsonLikeValue, String)
parseJLMessage msg = parse parseJLValue msg 

parseJLValue :: Parser JsonLikeValue
parseJLValue = parseJLString <|> parseJLInt <|> parseJLArray <|> parseJLMap

parseJLMap :: Parser JsonLikeValue 
parseJLMap = do
    str <- lift get 
    case str of
        ('d':'e':t) -> do
            lift $ put t
            return $ JLMap []
        ('d':t) -> parseJLMap' [] str
        _ -> throwE "Not a Map. "

parseJLMap' :: [(String, JsonLikeValue)] -> String -> Parser JsonLikeValue 
parseJLMap' acc str = 
    let
        body = if (head str == 'd') then (drop 1 str) else str
        (eitherJLString, noKeyRest) = parse parseJLString body
    in
        case eitherJLString of
            Left a -> throwE a
            Right (JLString key) ->
                let
                    (eitherParsedVal, rest) = parse parseJLValue noKeyRest
                in 
                    case eitherParsedVal of
                        Left a -> throwE a
                        Right parsedVal ->
                            let 
                                newAcc = (acc ++ [(key, parsedVal)])
                            in
                                if length rest == 0
                                    then throwE "Error Map parser: symbol : Map didn't close. "
                                    else
                                    case rest of
                                        ('e': t) -> do
                                            lift $ put t
                                            return $ JLMap newAcc
                                        _ -> parseJLMap' newAcc rest
                                        
parseJLArray :: Parser JsonLikeValue 
parseJLArray = do
    str <- lift get 
    case str of
        ('l':'e':t) -> do
            lift $ put t
            return $ JLArray []
        ('l':t) -> parseJLArray' [] t
        _ -> throwE "Not an Array. "

parseJLArray' :: [JsonLikeValue] -> String -> Parser JsonLikeValue
parseJLArray' acc str = 
    let
        body = str
        (eitherParsedVal, rest) = parse parseJLValue body
    in
        case eitherParsedVal of
            Left a -> throwE a
            Right parsedVal ->
                let 
                    newAcc = (acc ++ [parsedVal])
                in
                    if length rest == 0
                        then throwE "Error Array parser: symbol : Array didn't close. "
                        else
                        case rest of
                            ('e': t) -> do
                                lift $ put t
                                return $ JLArray newAcc
                            _ -> parseJLArray' newAcc rest

parseJLString :: Parser JsonLikeValue
parseJLString = do
    str <- lift get
    case takeWhile isDigit str of
        "0" -> do
            lift $ put $ drop 2 str
            return $ JLString ""
        "" -> throwE "Not a String. "
        _ -> parseJLString' str

parseJLString' :: String -> Parser JsonLikeValue
parseJLString' str =
    if isDigit $ head str then
        let
            strLen = takeWhile isDigit str
        in
        case drop (length strLen) str of
            (':':r) -> 
                let
                    parsedVal = take (read strLen) r           
                    rest = drop (read strLen) r
                in 
                    do
                        lift $ put rest
                        return $ JLString parsedVal
            _ -> throwE "Error String parser: symbol : wasn't found after a number. "
    else throwE "Error String parser: first symbol isn't a number. "

parseJLInt :: Parser JsonLikeValue
parseJLInt = 
    do 
        str <- lift get
        case str of
            ('i':t) -> parseJLInt' str
            _ -> throwE "Not an Int. "
        
parseJLInt' :: String -> Parser JsonLikeValue
parseJLInt' str = 
    case str of
        ('i':t) -> 
            if not (isDigit $ head t) then
                throwE "Error Int parser: next symbol after i isn't parseJLInt digit. "
            else
                let
                    prefix = takeWhile isDigit t
                    postfix = drop (length prefix) t
                in
                    case postfix of
                        ('e':r) -> 
                            let
                                parsedVal = read prefix
                            in
                                do
                                    lift $ put r
                                    return $ JLInt parsedVal
                        _ -> throwE "Error Int parser: int didn't end with e. "
        [] -> throwE "Error Int parser: Received an empty string. "
        _  -> throwE "Error Int parser: Received string doesn't start with i. "
        
------------------------------------------------------------------
------------------- Find Strings with paths ----------------------
------------------------------------------------------------------

msgToTops5 msg = 
    let 
        eitherJLValue = fst $ parseJLMessage msg
    in
        case eitherJLValue of
            Right jlVal -> tops5 $ findAllJLStringsIn jlVal
            _ -> error "Left in run"

findAllJLStringsIn :: JsonLikeValue -> [(JsonLikeValue, String)]
findAllJLStringsIn val = 
    case val of
        JLInt _ -> []
        JLString a -> [(val, "")]
        JLMap a -> findAllJLStringsInMap val
        JLArray a -> findAllJLStringsInArray val

findAllJLStringsInMap :: JsonLikeValue -> [(JsonLikeValue, String)]
findAllJLStringsInMap val = 
    case val of
        JLMap _ -> findAllJLStringsInMap' val []
        _ -> error "error findAllJLStringsInMap, value isnt a map"

findAllJLStringsInMap' :: JsonLikeValue -> [(JsonLikeValue, String)] -> [(JsonLikeValue, String)]
findAllJLStringsInMap' theMap acc =
    case theMap of 
        JLMap [] -> acc
        JLMap (h:t) -> 
            let
                key = fst h
                value = snd h
                stringsInHead = map (\(val, path) -> (val, "." ++ key ++ path)) (findAllJLStringsIn value)
            in
                findAllJLStringsInMap' (JLMap t) (acc ++ stringsInHead) 
        _ -> error "findAllJLStringsInMap' error not a map"

findAllJLStringsInArray :: JsonLikeValue -> [(JsonLikeValue, String)]
findAllJLStringsInArray val = 
    case val of
    JLArray _ -> findAllJLStringsInArray' val [] 0
    _ -> error "error findAllJLStringsInArray, value isnt an array"

findAllJLStringsInArray' :: JsonLikeValue -> [(JsonLikeValue, String)] -> Int -> [(JsonLikeValue, String)]
findAllJLStringsInArray' theArray acc index =
    case theArray of 
            JLArray [] -> acc
            JLArray (h:t) -> 
                let
                    stringsInHead = map (\(val, path) -> (val, "[" ++ show index ++ "]" ++ path)) (findAllJLStringsIn h)
                in
                    findAllJLStringsInArray' (JLArray t) (acc ++ stringsInHead) (index + 1)
            _ -> error "findAllJLStringsInMap' error not a map"

        
------------------------------------------------------------------
----------------------------- TOPS5 ------------------------------
------------------------------------------------------------------


tops5 :: [(JsonLikeValue, String)] -> [Maybe (JsonLikeValue, String)]
tops5 arr = 
    let
        (top1, rest1) = sepLongestJLString arr
        (top2, rest2) = sepLongestJLString rest1
        (top3, rest3) = sepLongestJLString rest2
        (top4, rest4) = sepLongestJLString rest3
        (top5, rest5) = sepLongestJLString rest4
    in
        [top1] ++ [top2] ++ [top3] ++ [top4] ++ [top5]

sepLongestJLString :: [(JsonLikeValue, String)] -> (Maybe (JsonLikeValue, String), [(JsonLikeValue, String)])
sepLongestJLString arr =
    let 
        max = findLongestJLStr arr
        rest = removeIfCan max arr
    in
        (max, rest)

removeIfCan :: Maybe (JsonLikeValue, String) -> [(JsonLikeValue, String)] ->[(JsonLikeValue, String)]
removeIfCan del arr =
    case del of 
        Nothing -> arr
        Just a -> delete a arr 

findLongestJLStr :: [(JsonLikeValue, String)] -> Maybe (JsonLikeValue, String)
findLongestJLStr [] = Nothing
findLongestJLStr (el1 : []) = Just el1
findLongestJLStr ((str1, path1) : (str2, path2) : t)
    | isLonger str1 str2 = findLongestJLStr ((str1, path1) : t)
    | otherwise = findLongestJLStr ((str2, path2) : t)

isLonger :: JsonLikeValue -> JsonLikeValue -> Bool
isLonger x y = 
    case x of
        JLString a ->
            case y of
                JLString b 
                    | (length a) > (length b) -> True
                    | otherwise -> False
                _ -> error "Not JLString" 
        _ -> error "Not JLString"

------------------------------------------------------------------
----------------------------- Output ------------------------------
------------------------------------------------------------------

strArrToIO :: [String] -> IO()
strArrToIO str = putStr $ unlines str

createStrForOutput :: [(JsonLikeValue, String)] -> [String]
createStrForOutput arr = map tupleStringAndPathIntoOutString arr

tupleStringAndPathIntoOutString :: (JsonLikeValue, String) -> String
tupleStringAndPathIntoOutString a = 
    let
        root = "root"
    in
        case a of
            (JLString val, path) -> root ++ path ++ " = \"" ++ val ++ "\""
            _ -> "Mano non-exhaustive in tupleStringAndPathIntoOutString"
        
simplifyMaybe :: [Maybe (JsonLikeValue, String)] -> [(JsonLikeValue, String)]
simplifyMaybe arr =
    let
        temp = fmap (\a -> simplifyMaybe' a) arr
    in
        (temp !! 0) ++ (temp !! 1) ++ (temp !! 2) ++ (temp !! 3) ++ (temp !! 4)

simplifyMaybe' :: Maybe (JsonLikeValue, String) -> [(JsonLikeValue, String)]
simplifyMaybe' x = 
    case x of
        Nothing -> []
        Just a -> [a]
