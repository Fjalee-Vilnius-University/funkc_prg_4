import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

type Parser a = ExceptT String (State String) a

test = 4