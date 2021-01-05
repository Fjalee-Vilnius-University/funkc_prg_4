module Main where

import Lib
import Task4

main :: IO ()
main = do
    msg <- getLine
    strArrToIO [""]
    strArrToIO $ createStrForOutput $ simplifyMaybe $ msgToTops5 msg
