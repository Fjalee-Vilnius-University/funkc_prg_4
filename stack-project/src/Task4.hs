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


p msg = parseJLMessage msg

t = 
    let
        msg1 = "d2:xsl1:01:11:21:01:11:21:01:11:2e2:ysl1:01:01:01:11:11:11:21:21:2e2:vsl1:X1:X1:O1:X1:O1:X1:X1:O1:Oee"
        msg2 = "d4:lastd2:vsl1:Xe2:ysli1ee2:xsli1eee4:prevd4:lastd2:vsl1:Oe2:ysli0ee2:xsli0eee4:prevd4:prevd4:lastd2:vsl1:Oe2:ysli0ee2:xsli1eee4:prevd4:lastd2:vsl1:Xe2:ysli0ee2:xsli1eeeee4:lastd2:vsl1:Xe2:ysli1ee2:xsli0eeeeee"
        msg3 = "d4:prevd4:lastld4:datali0ei1e1:Oeee4:prevd4:lastld4:datali0ei2e1:Xeee4:prevd4:lastld4:datali2ei0e1:Oeee4:prevd4:prevd4:lastld4:datali2ei2e1:Oeee4:prevd4:prevd4:lastld4:datali0ei0e1:Oeee4:prevd4:lastld4:datali1ei1e1:Xeeeee4:lastld4:datali2ei1e1:Xeeeee4:lastld4:datali1ei2e1:Xeeeeeee4:lastld4:datali1ei0e1:Xeeee"
    
        test1 = p msg1
        test2 = p msg2
        test3 = p msg3
        allTestRight = 
            case test1 of
                (Left _, _) -> False
                (Right _, _) ->
                    case test2 of 
                    (Left _, _) -> False
                    (Right _, _) ->
                        case test3 of
                            (Left _, _) -> False
                            (Right _, _) -> True
    in
        do
            putStrLn "======================================================================================================================================================================================================================================================================================"
            putStrLn $ show test1
            putStrLn ""
            putStrLn "2"
            putStrLn $ show test2
            putStrLn ""
            putStrLn "3"
            putStrLn $ show test3
            putStrLn ""
            putStrLn $ show allTestRight

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
------------------------------         ---------------------------
------------------------------------------------------------------
run = 
    let 
        --msg = "d4:prevd4:prevd4:lastld4:datali1ei0e1:Xeee4:prevd4:prevd4:lastld4:datali1ei2e1:Xeee4:prevd4:prevd4:lastld4:datali0ei1e1:Xeee4:prevd4:prevd4:lastld4:datali2ei2e1:Xeeee4:lastld4:datali2ei1e1:Oeeeee4:lastld4:datali2ei0e1:Oeeeee4:lastld4:datali1ei1e1:Oeeeee4:lastld4:datali0ei0e1:Oeeee4:lastld4:datali0ei2e1:Xeeee"
        
        --[(_, "oxi"),(_, 42),(_, "oxoxox")]
        --msg = "d3:qws3:oxi2:poi42e4:opep6:oxoxoxe"

        --[(_, 32),(_, 42),(_, 67)]
        --msg = "d3:qwsi32e2:poi42e4:opepi67ee"

        --[(_, 32),(_, 42),(_, "hyhy")]
        --msg = "d3:qwsi32e2:poi42e4:opep4:hyhye"

        --[45, 88, "nono", 65, "asd"]
        --msg = "li45ei88e4:nonoi65e3:asde"

        --[(_, "oxi"),(_, 42),(_, "oxoxox"), (_, [46, "asd"])]
        --msg = "d3:qws3:oxi2:poi42e4:opep6:oxoxox2:tyli43e3:asdee"

        --[("t0", "oxi"),("t1", 42), ("t2", [("t21", "mimi"), ("t22", 69)]),("t3", "oxoxox")]
        msg = "d2:t03:oxi2:t1i42e2:t2d3:t214:mimi3:t22i69ee2:t36:oxoxoxe"

        eitherJLValue = fst $ p msg
    in
        case eitherJLValue of
            --Left _ -> error "run function Left"
            Right jlVal -> findAllJLStringsIn jlVal
            _ -> error "asfdfgdsfsdfdsfsdfdsfsdf"
           -- Left _ -> error "run function Left"
            --Right jlVal -> findAllJLStrings jlVal
            -- Right jlVal -> myFind arrayFindJLString jlVal

type Finder a = ExceptT String (State JsonLikeValue) a

myFind :: Finder a -> JsonLikeValue -> (Either String a, JsonLikeValue)
myFind finder = runState (runExceptT finder)

findAllJLStringsIn :: JsonLikeValue -> [(JsonLikeValue, String)]
findAllJLStringsIn val = 
    case val of
        JLInt _ -> []
        JLString a -> [(val, "")]
        JLMap a -> findAllJLStringsInMap val
        JLArray a -> error "not implemented"

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
                stringsInHead = map (\(val, path) -> (val, key ++ "." ++ path)) (findAllJLStringsIn value)
            in
                findAllJLStringsInMap' (JLMap t) (acc ++ stringsInHead) 
        _ -> error "findAllJLStringsInMap' error not a map"






-- containerFindAllJLStrings :: JsonLikeValue -> [(JsonLikeValue, String)]
-- containerFindAllJLStrings container = containerFindAllJLStrings' container []

-- containerFindAllJLStrings' :: JsonLikeValue -> [(JsonLikeValue, String)] -> [(JsonLikeValue, String)]
-- containerFindAllJLStrings' container acc = 
--     case container of 
--         JLMap body -> 
--             case body of
--                 [] -> acc
--                 _ ->
--                     let
--                         (eitherJLString, rest) = myFind findNextMapValBodyStrings container
--                     in
--                         case eitherJLString of
--                             Left _ -> acc
--                             Right a -> containerFindAllJLStrings' rest (acc ++ a)
--         JLArray body ->
--             case body of
--                 [] -> acc
--                 _ ->
--                     let
--                         (eitherJLString, rest) = myFind arrayFindJLString container
--                     in
--                         case eitherJLString of
--                             Left _ -> acc
--                             Right a -> containerFindAllJLStrings' rest (acc ++ a)

-- findNextMapValBodyStrings :: Finder [(JsonLikeValue, String)]
-- findNextMapValBodyStrings = 
--     do 
--         myMap <- lift get
--         case myMap of
--             JLMap _ -> findNextMapValBodyStrings' myMap
--             _ -> throwE "Error Finder: Not a map. "

-- findNextMapValBodyStrings' :: JsonLikeValue -> Finder [(JsonLikeValue, String)]
-- findNextMapValBodyStrings' (JLMap []) = do
--     lift $ put $ JLMap []
--     throwE "No String in the map. "
-- findNextMapValBodyStrings' (JLMap ((key1, val1):t)) = 
--     case val1 of
--         JLString _ -> do
--             lift $ put $ JLMap t
--             return [(val1, "." ++ key1)]
--         JLMap a -> 
--             let
--                 stringsInMap = map addPathPrefix a
--             in
--                 do
--                     lift $ put $ JLMap t
--                     return 
--         _ -> findNextMapValBodyStrings' (JLMap t)

-- arrayFindJLString :: Finder (JsonLikeValue, String)
-- arrayFindJLString =
--     do 
--         myArray <- lift get
--         case myArray of
--             JLArray _ -> arrayFindJLString' myArray 0
--             _ -> throwE "Error Finder: Not an array. "

-- arrayFindJLString' :: JsonLikeValue -> Int -> Finder (JsonLikeValue, String)
-- arrayFindJLString' (JLArray []) elIndex = do
--     lift $ put $ JLArray []
--     throwE "No String in the array. "
-- arrayFindJLString' (JLArray (val1:t)) elIndex = 
--     case val1 of
--         JLString _ -> do
--             lift $ put $ JLArray t
--             return (val1, "[" ++ show elIndex ++ "]")
--         _ -> arrayFindJLString' (JLArray t) (elIndex + 1)




-- findAllJLStrings :: JsonLikeValue -> [(JsonLikeValue, String)]
-- findAllJLStrings jlValue = findAllJLStrings' jlValue []

-- findAllJLStrings' :: JsonLikeValue -> [(JsonLikeValue, String)] -> [(JsonLikeValue, String)]
-- findAllJLStrings' jlValue acc = 
--     case jlValue of
--         JLMap (h:t) ->
--             let
--                 findAllJLStrings h
--             in
                
--         JLArray (h:t) ->
--             let
--                 findAllJLStrings h
--             in

--         JLString _ -> 
--         JLInt _ -> findAllJLStrings' t acc 
--         _ -> error "findAllJLStrings' function mano-non-exhaustive pattern" 