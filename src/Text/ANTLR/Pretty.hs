{-# LANGUAGE FlexibleInstances, DefaultSignatures, UndecidableInstances #-}
module Text.ANTLR.Pretty where
import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict as M
import Data.Data (toConstr, Data(..))

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
  { indent   :: Int    -- indentation level
  , vis_chrs :: Int        -- number of visible characters
  , str :: String      -- constructed String
  , columns_soft :: Int
  , columns_hard :: Int
  , curr_col :: Int
  , curr_row :: Int
  }

-- The pretty state monad
type PrettyM val = State PState val

type Pretty = PrettyM ()

class Prettify t where
  {-# MINIMAL prettify #-}

  prettify :: t -> Pretty
  default prettify :: (Show t) => t -> Pretty
  prettify = rshow

  prettifyList :: [t] -> Pretty
  prettifyList = prettifyList_

-- Initial Pretty state
initPState = PState
  { indent       = 0   -- Indentation level
  , vis_chrs     = 0   -- Number of visible characters consumed.
  , str          = ""  -- The string
  , columns_soft = 80  -- Soft limit on terminal width.
  , columns_hard = 120 -- Hard limit on terminal width.
  , curr_col     = 0   -- Column position in the current row.
  , curr_row     = 0   -- Number of newlines seen
  }

-- Prettify a string by putting it on the end of the current string state
pLine :: String -> Pretty
pLine s = do
  pStr s
  _pNewLine

-- Currently assuming all input strings contain no newlines, and that this is
-- only called on relatively small strings because strings running over the end
-- of the hard column limit get dumped onto the next line *no matter what*.
-- Strings can run over the soft limit, but hitting the soft limit after a call
-- to pStr forces a newline.
pStr :: String -> Pretty
pStr s = do
  pstate <- get
  _doIf _pNewLine (length s + curr_col pstate > columns_hard pstate && curr_col pstate /= 0)
  pstate <- get
  _doIf _pIndent  (curr_col pstate == 0 && indent pstate > 0)
  pstate <- get
  put $ pstate
    { str = str pstate ++ s
    , curr_col = (curr_col pstate) + length s
    }
  pstate <- get
  _doIf _pNewLine (curr_col pstate > columns_soft pstate)

-- TODO ...
pChr :: Char -> Pretty
pChr c = pStr [c]

-- Get rid of if-then-else lines in the Pretty monad:
_doIf fncn True  = fncn
_doIf fncn False = return ()

-- Indent by the number of spaces specified in the state
_pIndent :: Pretty
_pIndent = do
  pstate <- get
  put $ pstate
    { str      = str pstate ++ replicate (indent pstate) ' '
    , curr_col = curr_col pstate + indent pstate
    , vis_chrs = vis_chrs pstate + indent pstate
    }

-- Insert a newline
_pNewLine :: Pretty
_pNewLine = do
  pstate <- get
  put $ pstate
    { str = str pstate ++ "\n"
    , curr_col = 0
    , curr_row = curr_row pstate + 1
    }

pshow :: (Prettify t) => t -> String
pshow t = str $ execState (prettify t) initPState

pshowIndent :: (Prettify t) => Int -> t -> String
pshowIndent i t = str $ execState (prettify t) $ initPState { indent = i }

-- Plain-vanilla show of something:
rshow :: (Show t) => t -> Pretty
rshow t = do
  pstate <- get
  let s = show t
  put $ pstate
    { str      = str pstate ++ s
    , curr_row = curr_row pstate + (length . filter (== '\n')) s
    , curr_col = curr_col pstate -- TODO
    }

pParens fncn = do
  pStr "("
  fncn
  pStr ")"

incrIndent :: Int -> Pretty
incrIndent n = do
  pstate <- get
  put $ pstate { indent = indent pstate + n }

setIndent :: Int -> Pretty
setIndent n = do
  pstate <- get
  put $ pstate { indent = n }

-- Prettify the given value and compute the number of characters consumed as a
-- result.
pCount :: (Prettify v) => v -> PrettyM Int
pCount v = do
  i0 <- indent <$> get
  prettify v
  i1 <- indent <$> get
  return (i1 - i0)

-- Put 
pListLines :: (Prettify v) => [v] -> Pretty
pListLines vs = do
  pStr "[ "
  col0 <- curr_col <$> get
  i0   <- indent   <$> get
  setIndent (col0 - 2)
  sepBy (pLine "" >> pStr ", ") (map prettify vs)
  pLine "" >> pStr "]"
  setIndent i0 -- Reset indentation back to what it was

instance (Prettify k, Prettify v) => Prettify (M.Map k v) where
  prettify m = do
    -- (5 == length of "Map: ") ==> TODO: indentation "discipline"
    pStr "Map: "; incrIndent 5
    prettify $ M.toList m -- TODO: prettier map
    incrIndent (-5)

instance (Prettify v) => Prettify (Maybe v) where
  prettify Nothing  = pStr "Nope"
  prettify (Just v) = pStr "Yep" >> pParens (prettify v)

prettifyList_ [] = pStr "[]"
prettifyList_ vs = do
  pStr "["
  sepBy (pStr ", ") (map prettify vs)
  pStr "]"

instance (Prettify v) => Prettify [v] where
  prettify = prettifyList 

-- TODO: template haskell-ify for larger tuples
instance (Prettify a, Prettify b) => Prettify (a,b) where
  prettify (a,b) = do
    pStr "("
    prettify a
    pStr ","
    prettify b
    pStr ")"

instance (Prettify a, Prettify b, Prettify c) => Prettify (a,b,c) where
  prettify (a,b,c) = do
    pStr "("
    prettify a
    pStr ","
    prettify b
    pStr ","
    prettify c
    pStr ")"

instance (Prettify a, Prettify b, Prettify c, Prettify d) => Prettify (a,b,c,d) where
  prettify (a,b,c,d) = do
    pStr "("
    prettify a
    pStr ","
    prettify b
    pStr ","
    prettify c
    pStr ","
    prettify d
    pStr ")"

sepBy s [] = return ()
sepBy s (v:vs) = foldl (_sepBy s) v vs

_sepBy s ma mb = ma >> s >> mb 

instance Prettify Char where
  prettify = pChr
  prettifyList = pStr

instance Prettify () where prettify = rshow
instance Prettify Bool where prettify = rshow
instance Prettify Int where prettify = rshow

--class BoundedEnum a where
--  showConstr :: a -> String
  --default showConstr :: (Data a) => a -> String
  --showConstr = show . toConstr

--instance (BoundedEnum a) => Prettify a where
--  prettify a = show $ toConstr a

