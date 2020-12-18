import System.Environment
import Data.Monoid
import Data.Functor
import Data.Semigroup
import Control.Monad

test1 = mplus Nothing (Just 42)

addOne :: Int -> Int
addOne a = a + 1

test2 = do
    a <- addOne 1
    b <- addOne 2
    return (a,b)



-- data JsonLikeValue =
--     JLString String |
--     JLInt Int |
--     JLMap [(String, JsonLikeValue)] |
--     JLArray [JsonLikeValue]
--     deriving (Show, Eq)
    
-- parseJLInt :: String -> Either String (JsonLikeValue, String)
-- parseJLInt ('i':t) = 
--         if not (isDigit $ head t) then
--             Left "Error Int parser: next symbol after i isn't a digit"
--         else
--             let
--                 prefix = takeWhile isDigit t
--                 postfix = drop (length prefix) t
--             in
--                 case postfix of
--                     ('e':r) -> 
--                         let
--                             parsedVal = read prefix
--                             rest = r
--                         in
--                             Right (JLInt parsedVal, rest)
--                     _ -> Left "Error Int parser: int didn't end with e"
-- parseJLInt []  = Left "Error Int parser: Received an empty string"
-- parseJLInt _  = Left "Error Int parser: Received string doesn't start with i"


-- parseJLString :: String -> Either String (JsonLikeValue, String)
-- parseJLString str =
--     if isDigit $ head str then
--         let
--             strLen = takeWhile isDigit str
--         in
--         case drop (length strLen) str of
--             (':':r) -> 
--                 let
--                     parsedVal = take (read strLen) r           
--                     rest = drop (read strLen) r
--                 in
--                 Right (JLString parsedVal, rest)
--             _ -> Left "Error String parser: symbol : wasn't found after a number"
--     else Left "Error String parser: first symbol isn't a number"