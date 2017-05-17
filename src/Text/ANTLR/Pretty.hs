{-# LANGUAGE FlexibleInstances, DefaultSignatures #-}
module Text.ANTLR.Pretty where
import Control.Monad.Trans.State.Lazy

{- I want to have something like Show whereby every time I add a new type to the
 - system, I can implement a function that gets called by existing code which
 - happens to have types that get parametrized by that type. I don't want to
 - modify an existing file / centralizing all of the types in my system into a
 - single file makes little sense because then that one file becomes a hub /
 - single point of failure.
 - * I need a typeclass (no modifying existing files, but they need to call my
 - new code without passing around a new show function)
 - * The prettify function of that typeclass needs to return a state monad so
 - that recursive calls keep the state
 - * A pshow function needs to evalState on the prettify function with an
 - initial indentation of zero (along with any other future state values...)
 -}

data PState = PState
  { i :: Int -- indentation level
  }

type Pretty s = State PState s

class Prettify t where
  prettify :: t -> Pretty String
  default prettify :: (Show t) => t -> Pretty String
  prettify = rshow

-- Initial Pretty state
initPState = PState
  { i = 0
  }

pshow :: (Prettify t) => t -> String
pshow t = evalState (prettify t) initPState

-- Plain-vanilla show of something:
rshow :: (Show t) => t -> Pretty String
rshow = return . show

{-
indent n fncn = do
  i <- get
  put (i + n)
  fncn
  put i
-}
{-
class PShow t where
  pshow :: t -> Pretty

newtype Pretty = Pretty (State Int String)

--evalState st 0

indent fncn = do
  i <- get
  s <- fncn
  return $ replicate i ' ' ++ s

instance PShow Bool   where pshow b  = show b
instance PShow [Char] where pshow cs = show cs
-}
