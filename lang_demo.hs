{-#LANGUAGE TypeFamilies #-}
module LangDemo where

-- If you never change the base value and are only adding expressions
data BaseVal  = IntV Int

data BaseExp  = ValE BaseVal
              | AddE BaseExp BaseExp

data ExtExp   = BaseE BaseExp
              | SubE ExtExp ExtExp



class Lang lang where
  eval :: lang -> BaseVal

instance Lang BaseExp where
  eval (ValE i) = i
  eval (AddE be1 be2) =
    let IntV i1 = eval be1
        IntV i2 = eval be2
    in  IntV $ i1 + i2

instance Lang ExtExp where
  eval (BaseE b) = eval b
  eval (SubE ee1 ee2) =
    let IntV i1 = eval ee1
        IntV i2 = eval ee2
    in  IntV $ i1 - i2

-- if you want to be able to change the set of values you can evaluate to when extending the language

data BaseVal2 = IntV2 Int

data BaseExp2 = ValBE2 BaseVal2
              | AddE2 BaseExp2 BaseExp2

data ExtVal2  = StringV2 String

data ExtExp2  = BaseE2 BaseExp2
              | ValEE2 ExtVal2
              | ConcatE2 ExtExp2 ExtExp2

-- can use type families to suspend the value kind until later.
data family Val2

class Lang2 lang where
  eval :: lang -> Val2

-- define at some time t1 for the base language
data instance Val2 = BV2 BaseVal2

instance Lang2 BaseExp2 where
  eval ValBE2 bv2 = BV2 bv2
  eval AddE2 be1 be2 =
    let IntV2 i1 = eval be1
        IntV2 i2 = eval be2
    in  BV2 $ IntV2 $ i1 + i2


-- extend at some later time t2 for the extended language
data instance Val2 = EV2 ExtVal2

instance Lang2 ExtExp2 where
  eval BaseE2 be2 = eval be2
  eval ValEE2 ev2 = ev2
  eval ConcatE2 ee1 ee2 =
    case (eval ee1, eval ee2) of
    (StringV2 s1, StringV2 s2) -> EV2 $ StringV2 $ s1 ++ s2
    _                          -> error "trying to concat something other than strings!"
    
