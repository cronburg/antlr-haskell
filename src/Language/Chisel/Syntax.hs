module Language.Chisel.Syntax where

data ChiselProd = ChiselProd
  { count     :: Maybe SizeArith
  , formals   :: Maybe [Formal]
  , magnitude :: Maybe Magnitude
  , alignment :: Maybe SizeArith
  , rhs       :: Group
  }

type Formal = String

--data Magnitude = 

