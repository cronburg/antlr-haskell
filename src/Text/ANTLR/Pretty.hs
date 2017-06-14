{-# LANGUAGE FlexibleInstances, DefaultSignatures, UndecidableInstances
    , OverloadedStrings #-}
module Text.ANTLR.Pretty where
import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict as M
import Data.Data (toConstr, Data(..))

import qualified Data.Text as T

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
  , vis_chrs :: Int    -- number of visible characters
  , str :: T.Text      -- constructed T.Text
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
  , str          = T.empty -- The string
  , columns_soft = 60  -- Soft limit on terminal width.
  , columns_hard = 80  -- Hard limit on terminal width.
  , curr_col     = 0   -- Column position in the current row.
  , curr_row     = 0   -- Number of newlines seen
  }

-- Prettify a string by putting it on the end of the current string state
pLine :: T.Text -> Pretty
pLine s = do
  pStr s
  _pNewLine

pStr' :: String -> Pretty
pStr' = pStr . T.pack

-- Currently assuming all input strings contain no newlines, and that this is
-- only called on relatively small strings because strings running over the end
-- of the hard column limit get dumped onto the next line *no matter what*.
-- T.Texts can run over the soft limit, but hitting the soft limit after a call
-- to pStr forces a newline.
pStr :: T.Text -> Pretty
pStr s = do
  pstate <- get
  _doIf _pNewLine (T.length s + curr_col pstate > columns_hard pstate && curr_col pstate /= 0)
  pstate <- get
  _doIf _pIndent  (curr_col pstate == 0 && indent pstate > 0)
  pstate <- get
  put $ pstate
    { str = T.append (str pstate) s
    , curr_col = (curr_col pstate) + T.length s
    }
  pstate <- get
  _doIf _pNewLine (curr_col pstate > columns_soft pstate)

-- TODO ...
pChr :: Char -> Pretty
pChr c = pStr $ T.singleton c

-- Get rid of if-then-else lines in the Pretty monad:
_doIf fncn True  = fncn
_doIf fncn False = return ()

-- Indent by the number of spaces specified in the state
_pIndent :: Pretty
_pIndent = do
  pstate <- get
  put $ pstate
    { str      = str pstate `T.append` T.replicate (indent pstate) (T.singleton ' ')
    , curr_col = curr_col pstate + indent pstate
    , vis_chrs = vis_chrs pstate + indent pstate
    }

-- Insert a newline
_pNewLine :: Pretty
_pNewLine = do
  pstate <- get
  put $ pstate
    { str = T.snoc (str pstate) '\n'
    , curr_col = 0
    , curr_row = curr_row pstate + 1
    }

pshow :: (Prettify t) => t -> T.Text
pshow t = str $ execState (prettify t) initPState

pshow' :: (Prettify t) => t -> String
pshow' = T.unpack . pshow

pshowIndent :: (Prettify t) => Int -> t -> T.Text
pshowIndent i t = str $ execState (prettify t) $ initPState { indent = i }

-- Plain-vanilla show of something:
rshow :: (Show t) => t -> Pretty
rshow t = do
  pstate <- get
  let s = show t
  put $ pstate
    { str      = str pstate `T.append` T.pack s
    , curr_row = curr_row pstate + (T.length . T.filter (== '\n')) (T.pack s)
    , curr_col = curr_col pstate -- TODO
    }

pParens fncn = do
  pChr '('
  fncn
  pChr ')'

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
  pStr $ T.pack "[ "
  col0 <- curr_col <$> get
  i0   <- indent   <$> get
  setIndent (col0 - 2)
  sepBy (pLine T.empty >> (pStr $ T.pack ", ")) (map prettify vs)
  pLine T.empty >> pChr ']'
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
  pChr '['
  sepBy (pStr ", ") (map prettify vs)
  pChr ']'

instance (Prettify v) => Prettify [v] where
  prettify = prettifyList 

-- TODO: template haskell-ify for larger tuples
instance (Prettify a, Prettify b) => Prettify (a,b) where
  prettify (a,b) = do
    pChr '('
    prettify a
    pChr ','
    prettify b
    pChr ')'

instance (Prettify a, Prettify b, Prettify c) => Prettify (a,b,c) where
  prettify (a,b,c) = do
    pChr '('
    prettify a
    pChr ','
    prettify b
    pChr ','
    prettify c
    pChr ')'

instance (Prettify a, Prettify b, Prettify c, Prettify d) => Prettify (a,b,c,d) where
  prettify (a,b,c,d) = do
    pChr '('
    prettify a
    pChr ','
    prettify b
    pChr ','
    prettify c
    pChr ','
    prettify d
    pChr ')'

sepBy s [] = return ()
sepBy s (v:vs) = foldl (_sepBy s) v vs

_sepBy s ma mb = ma >> s >> mb 

instance Prettify Char where
  prettify = pChr
  prettifyList = pStr . T.pack

instance Prettify () where prettify = rshow
instance Prettify Bool where prettify = rshow
instance Prettify Int where prettify = rshow

--class BoundedEnum a where
--  showConstr :: a -> T.Text
  --default showConstr :: (Data a) => a -> T.Text
  --showConstr = show . toConstr

--instance (BoundedEnum a) => Prettify a where
--  prettify a = show $ toConstr a

