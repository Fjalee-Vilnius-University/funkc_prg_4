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

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

parseJLValue :: Parser JsonLikeValue
parseJLValue = parseJLString <|> parseJLInt <|> parseJLArray <|> parseJLMap

parseJLMap :: Parser JsonLikeValue 
parseJLMap = do
    str <- lift get 
    case str of
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
        ('l':t) -> parseJLArray' [] str
        _ -> throwE "Not an Array. "

parseJLArray' :: [JsonLikeValue] -> String -> Parser JsonLikeValue
parseJLArray' acc str = 
    let
        body = if (head str == 'l') then (drop 1 str) else str
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

-- parseJLMap :: String -> String -> Either String (JsonLikeValue, String)
-- parseJLMap ('d':t) orgStr = 
--     case parseAllMapedJLValues t orgStr of
--         Left a -> Left a
--         Right (mapBody, rest) -> Right (mapBody, rest)
-- parseJLMap dnStartD orgStr = Left ("Error around character " ++ show errPos ++ " received string: " ++ dnStartD ++ " map has to start with a 'd'")
--     where 
--         errPos = lenDiff orgStr dnStartD

-- lenDiff :: String -> String -> Int
-- lenDiff str1 str2 = (length str1) - (length str2)

-- parseAllMapedJLValues :: String -> String -> Either String (JsonLikeValue, String)
-- parseAllMapedJLValues ('e':t) _ = Right (JLMap [], t)
-- parseAllMapedJLValues str orgStr = 
--     case parseMapedJLValue str orgStr of
--         Left a -> Left a
--         Right ((key, value), rest) -> 
--             case parseAllMapedJLValues rest orgStr of
--                 Left a -> Left a
--                 Right (JLMap acc, rest1) -> Right $ (JLMap ([(key, value)] ++ acc), rest1)


-- parseMapedJLValue :: String -> String -> Either String ((String, JsonLikeValue), String)
-- parseMapedJLValue str orgStr = 
--     case parseString str orgStr of
--         Left a -> Left a
--         Right (key, rest) ->
--             case parseJLValue rest orgStr of
--                 Left a -> Left a
--                 Right (value, rest') -> Right ((key, value), rest')

-- parseString :: String -> String -> Either String (String, String)
-- parseString str orgStr =
--     let
--         errPos = lenDiff orgStr str
--         strLen = if C.isDigit $ head str
--             then L.takeWhile C.isDigit str
--             else "not declared"
--         postfix = L.drop (length strLen) str
--     in
--         case strLen of 
--             "not declared" -> Left ("Error around character " ++ show errPos ++ " received string: " ++ str ++ " Length of the string was not declared")
--             _ ->
--                 case postfix of
--                 (':':r) -> Right (L.take (read strLen) r, L.drop (read strLen) r)
--                 _ -> Left ("Error around character " ++ show errPos ++ " received string: " ++ str ++ " Invalid string")
