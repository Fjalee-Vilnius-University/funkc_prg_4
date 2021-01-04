module Main where

import Lib
import Task4

main :: IO ()
main = do
    msg <- getLine
    strArrToIO [""]
    strArrToIO $ createStrForOutput $ simplifyMaybe $ msgToTops5 msg--"li0ei4eld0:5:AGIeG5:0CZc72:m41:40:4:DMCWi2e5:MPoPI3:3YLele2:EQ1:3el1:E3:oPHi2eee"
