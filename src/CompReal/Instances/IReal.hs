{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CompReal.Instances.IReal (IReal.IReal) where

import Utils
import CompOrd
import CompReal
import Boundable
import Powers
import Limit

import qualified Data.Number.IReal as IReal
import qualified Data.Number.IReal.IReal as IReal (ir, appr)
import qualified Data.Number.IReal.IntegerInterval as IReal (IntegerInterval(..), upperI)

import GHC.Real (Ratio(..), (%))

--ireal is a bit unique, because it explicitely uses IReals to represent open real intervals
--as well as real numbers. This means an IReal value, which is a function Int->(Integer,Integer),
--isn't guaranteed to converge to a single number at all, which is anoying when typing functions in practice
--This CompReal instance assumes IReals are numbers (all intervals are "thin", that is, have
--a difference of 2) and as such always converge with the expected modulus
instance CompReal IReal.IReal where
    bound r n = let IReal.I (l,u) = IReal.appr r (n+1); d = pow2 (n+1) in (l % d, u % d)

instance Limit Rational IReal.IReal where
    limit f = IReal.ir (\i -> let (a :% b) = f (i+1) in fromInteger (pow2 i * a `div` b))

instance Limit IReal.IReal IReal.IReal where
    limit f = IReal.ir (\i -> let IReal.I (l,u) = IReal.appr (f i) (i+1) in IReal.I (l `div` 2, u `div` 2))
    errorLimit = errorLimitDef

instance CompOrd IReal.IReal where
    domCompare = domCompareDef
    compMin = min
    compMax = max

instance Powers IReal.IReal where
    pow = powDef IReal.appr IReal.ir IReal.scale

instance Boundable IReal.IReal


--we need to define these instances for IntegerInterval to use powDef
instance Powers IReal.IntegerInterval where
    --Powers is actually copied from ireal, so of course we can just use its pow
    pow = IReal.pow

instance Boundable IReal.IntegerInterval where
    bounds (IReal.I ii) = ii