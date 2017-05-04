module Text.ANTLR.Pretty where
import Control.Monad.State.Lazy

indent n fncn = do
  i <- get
  put (i + n)
  fncn
  put i

class PrettyShow a where
  pshow :: a -> Pretty a

