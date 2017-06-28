{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Language.Chisel.Syntax where
import Text.ANTLR.Pretty
import Text.ANTLR.Set (Hashable(..), Generic(..))

data ChiselProd = ChiselProd
  { prodID    :: UpperID
  , count     :: Maybe LowerID
  , formals   :: Maybe [Formal]
  , magnitude :: Maybe Magnitude
  , alignment :: Maybe SizeArith
  , rhs       :: Group
  }

prodFMA s f m a   = ChiselProd s Nothing  (Just f)  (Just m) (Just a)
prodF s f         = ChiselProd s Nothing  (Just f)  Nothing Nothing
prodMA s m a      = ChiselProd s Nothing  Nothing   (Just m) (Just a)
prodM s m         = ChiselProd s Nothing  Nothing   (Just m) Nothing
prodNMA s n m a   = ChiselProd s (Just n) Nothing   (Just m) (Just a)

type Formal = String

-- Whether or not '#' was used:
type Wild = Bool

data Magnitude = Mag Wild SizeArith

magWild = Mag True  -- Variable magnitude
magNorm = Mag False -- Fixed magnitude
magID   = Mag False . SizeID

type Alignment = SizeArith

type Group = [GroupExp]

-- Left False == no size annotation
-- Left True  == '#' annotation
-- Right sz   == fixed size annotation
type Count = Either Wild SizeArith

data GroupExp =
    GProd Count ChiselProd
  | GSize Wild  SizeArith
  | GFlags      [Flag]
  | GLabel      Label
  | GLabels     [Label]
  | GProdApp    SizeArith ProdApp

gProdWild     = GProd (Left True)
gProdNorm     = GProd (Left False)
gProdArith a  = GProd (Right a)

gSizeWild = GSize True
gSizeNorm = GSize False

type Flag = ProdID

type ProdApp = [ProdID]

type UpperID = String
type LowerID = String
type ProdID = String
type LabelID = String

data Label = Label LabelID LabelExp

data LabelExp =
    LProd     Wild ChiselProd
  | LProdApp  Wild ProdApp
  | LSize     Wild SizeArith

lProdWild = LProd True
lProd     = LProd False

lProdAppWild = LProdApp True
lProdApp     = LProdApp False

lSizeWild = LSize True
lSize     = LSize False

data SizeArith =
    SizeInt   Int
  | SizeID    LowerID
  | SizeExp   SizeArith SizeArith
  | SizeArith SizeArith Primitive

singleArith = SizeArith (SizeInt 1)

data Primitive = Page | Word | Byte | Bit
  deriving (Show, Eq, Ord, Generic, Hashable)

lexeme2prim "page"  = Just Page
lexeme2prim "pages" = Just Page
lexeme2prim "word"  = Just Word
lexeme2prim "words" = Just Word
lexeme2prim "byte"  = Just Byte
lexeme2prim "bytes" = Just Byte
lexeme2prim "bit"   = Just Bit
lexeme2prim "bits"  = Just Bit
lexeme2prim _       = Nothing

instance Read Primitive where
  readsPrec _ input = case lexeme2prim input of
    Just x  -> [(x,"")]
    Nothing -> []

instance Prettify Primitive where prettify = rshow

list a = [a]
cons = (:)
append = (++)

