{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module CompReal.Instances.ExactReal (AnyCReal) where

import CompOrd
import CompReal
import Boundable
import Powers
import Limit

import qualified Data.CReal as ExactReal
import qualified Data.CReal.Internal as ExactReal (crMemoize)

import GHC.Real (Ratio(..))
import GHC.TypeLits
import Data.Proxy
import Data.Bits


newtype AnyCReal = AnyCReal { anyCReal :: forall n. KnownNat n => ExactReal.CReal n }

getCReal :: (forall n. KnownNat n => ExactReal.CReal n -> a) -> AnyCReal -> Int -> a
getCReal f x n = case someNatVal $ fromIntegral n of
                    Just (SomeNat (_ :: Proxy _n)) -> let y :: ExactReal.CReal _n
                                                          y = (anyCReal x) @_n
                                                      in f y
                    Nothing -> error $ "Invalid accuracy " ++ show n

--since ExactReal.atPrecision works the same regardless of type-level precision, we can just pick any (I
--chose zero for no particular reason) and then call ExactReal.atPrecision with the desired precision
atPrecision :: AnyCReal -> Int -> Integer
atPrecision x i = getCReal (flip ExactReal.atPrecision i) x 0

mapCReal :: (forall n. KnownNat n => ExactReal.CReal n -> ExactReal.CReal n) -> AnyCReal -> AnyCReal
mapCReal f x = AnyCReal $ f $ anyCReal x

mapCReal2 :: (forall n. KnownNat n => ExactReal.CReal n -> ExactReal.CReal n -> ExactReal.CReal n) -> AnyCReal -> AnyCReal -> AnyCReal
mapCReal2 f x y = AnyCReal $ f (anyCReal x) (anyCReal y)

instance CompReal AnyCReal where
    approx x n = getCReal toRational x (n+1)

instance Num AnyCReal where
    (+) = mapCReal2 (+)
    (-) = mapCReal2 (-)
    (*) = mapCReal2 (*)
    negate = mapCReal negate
    abs = mapCReal abs
    signum = mapCReal signum
    fromInteger i = AnyCReal $ fromInteger i

instance Fractional AnyCReal where
    (/) = mapCReal2 (/)
    recip = mapCReal recip
    fromRational r = AnyCReal $ fromRational r

instance Floating AnyCReal where
    pi = AnyCReal pi
    exp = mapCReal exp
    log = mapCReal log
    sqrt = mapCReal sqrt
    (**) =  mapCReal2 (**)
    logBase = mapCReal2 logBase
    sin = mapCReal sin
    cos = mapCReal cos
    tan = mapCReal tan
    asin = mapCReal asin
    acos = mapCReal acos
    atan = mapCReal atan
    sinh = mapCReal sinh
    cosh = mapCReal cosh
    tanh = mapCReal tanh
    asinh = mapCReal asinh
    acosh = mapCReal acosh
    atanh = mapCReal atanh

--So, because exact-real has a dumb implementation of limits, we have to do it ourselves
--really tho. it uses equality to test if the value of the limit was reached. like, bruhh
instance Limit Rational AnyCReal where
    limit f = AnyCReal $ ExactReal.crMemoize (\i -> let (a :% b) = f i in roundD (shiftL a i) b)
        where roundD n d = case divMod n d of
                              (q, r) -> case compare (unsafeShiftL r 1) d of
                                LT -> q
                                EQ -> if testBit q 0 then q + 1 else q
                                GT -> q + 1

instance Limit AnyCReal AnyCReal where
    limit f = AnyCReal $ ExactReal.crMemoize (\i -> let x = f i in roundD (atPrecision x (i+1)) 2)
        where roundD n d = case divMod n d of
                              (q, r) -> case compare (unsafeShiftL r 1) d of
                                LT -> q
                                EQ -> if testBit q 0 then q + 1 else q
                                GT -> q + 1
    errorLimit = errorLimitDef

instance CompOrd AnyCReal where
    domCompare = domCompareDef
    compMin = mapCReal2 min
    compMax = mapCReal2 max

instance Powers AnyCReal where
    pow = powDef atPrecision (\f -> AnyCReal $ ExactReal.crMemoize f) scale
        where scale x n | n >= 0    = shift x n
                        | otherwise = shift (x + bit (-n-1)) n

instance Boundable AnyCReal


instance Show AnyCReal where
    show x = getCReal show x 80