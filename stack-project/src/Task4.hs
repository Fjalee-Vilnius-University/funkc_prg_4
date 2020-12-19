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
parseJLValue = parseJLString <|> parseJLInt <|> parseJLArray -- <|> parseJLMap

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
                        then throwE "Error Array parser: symbol : Array didn't close."
                        else
                        case rest of
                            ('e': t ) -> do
                                lift $ put t
                                return $ JLArray newAcc
                            
                            _ -> parseJLArray' newAcc rest

parseJLMap :: Parser JsonLikeValue 
parseJLMap = do
    str <- lift get 
    parseJLMap' str

parseJLMap' :: String -> Parser JsonLikeValue 
parseJLMap' str = throwE "Map not implemented. "

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

-- -- parseJLArray :: [JsonLikeValue] -> String -> String -> Either String (JsonLikeValue, String)
-- -- parseJLArray [] ('l':t) orgStr =
-- --     case parseJLValue t orgStr of
-- --         Left a -> Left a
-- --         Right (value, (fstCh : rest)) ->
-- --             case fstCh of
-- --                 'e' -> Right (JLArray ([] ++ [value]), rest)
-- --                 _ -> parseJLArray ([] ++ [value]) (fstCh : rest) orgStr
-- -- parseJLArray parsedArrEls (h:t) orgStr =
-- --     case parseJLValue (h:t) orgStr of
-- --         Left a -> Left a
-- --         Right (value, (fstCh : rest)) ->
-- --             case fstCh of
-- --                 'e' -> Right (JLArray (parsedArrEls ++ [value]), rest)
-- --                 _ -> parseJLArray (parsedArrEls ++ [value]) (fstCh : rest) orgStr
-- -- parseJLArray [] [] orgStr = Left ("Error around character " ++ show errPos ++ "Empty Array")
-- --     where
-- --         errPos = lenDiff orgStr []
-- -- parseJLArray _ dnStartL orgStr = Left ("Error around character " ++ show errPos ++ " received string: " ++ dnStartL ++ " list has to start with an 'l'")
-- --     where
-- --         errPos = lenDiff orgStr dnStartL
