import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import System.Environment

import Data.List
import Data.Char

--runState (runExceptT parseInt) "i432easd"
type Parser a = ExceptT String (State String) a

data JsonLikeValue =
    JLString String |
    JLInt Int |
    JLMap [(String, JsonLikeValue)] |
    JLArray [JsonLikeValue]
    deriving (Show, Eq)

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
--         strLen = if isDigit $ head str
--             then takeWhile isDigit str
--             else "not declared"
--         postfix = drop (length strLen) str
--     in
--         case strLen of 
--             "not declared" -> Left ("Error around character " ++ show errPos ++ " received string: " ++ str ++ " Length of the string was not declared")
--             _ ->
--                 case postfix of
--                 (':':r) -> Right (take (read strLen) r, drop (read strLen) r)
--                 _ -> Left ("Error around character " ++ show errPos ++ " received string: " ++ str ++ " Invalid string")

-- parseJLValue :: String -> Either String (JsonLikeValue, String)
-- parseJLValue ('d':t) orgStr = 
    -- case parseJLMap('d':t) orgStr of
    --     Left a -> Left a
    --     Right (a, b) -> Right (a, b)
-- parseJLValue ('l':t) orgStr = 
    -- case parseJLArray [] ('l':t) orgStr of
    --     Left a -> Left a
    --     Right (a, b) -> Right (a, b)
-- parseJLValue ('i':t) = 
--     case parseJLInt of
--         Left a -> Left a
--         Right (a, b) -> Right (a, b)
-- parseJLValue (h:t) =
--     if isDigit h
--     then 
--         case parseJLString (h:t) of
--             Left a -> Left a
--             Right (a, b) -> Right (a, b)
--     else Left ("Error JLV parser: JsonLikeValue has to start with a 'd' or a 'l' or an 'i' or a digit")
-- parseJLValue [] = Left "Error JLV parser: Received an empty string"

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

parseJLString' = do
    str <- lift get
    parseJLString str

parseJLString :: String -> Parser JsonLikeValue
parseJLString str = 
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
            _ -> throwE "Error String parser: symbol : wasn't found after a number"
    else throwE "Error String parser: first symbol isn't a number"


parseJLInt' = 
    do 
        str <- lift get
        parseJLInt str

parseJLInt :: String -> Parser JsonLikeValue
parseJLInt str = 
    case str of
        ('i':t) -> 
            if not (isDigit $ head t) then
                throwE "Error Int parser: next symbol after i isn't a digit"
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
                        _ -> throwE "Error Int parser: int didn't end with e"
        [] -> throwE "Error Int parser: Received an empty string"
        _  -> throwE "Error Int parser: Received string doesn't start with i"
