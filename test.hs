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