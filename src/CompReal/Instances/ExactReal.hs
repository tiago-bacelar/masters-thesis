{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module CompReal.Instances.ExactReal (AnyCReal) where

import Utils
import CompOrd
import CompReal
import Boundable
import Powers
import Limit

import qualified Data.CReal as ExactReal
import qualified Data.CReal.Internal as ExactReal (crMemoize, showAtPrecision)

import GHC.Real (Ratio(..), (%))
--import GHC.TypeLits
--import Data.Proxy
import Data.Bits

{-
--Sadly, using existential qualification makes exact-real's memoization not work
newtype AnyCReal = AnyCReal { anyCReal :: forall n. KnownNat n => ExactReal.CReal n }

getCReal :: (forall n. KnownNat n => ExactReal.CReal n -> a) -> AnyCReal -> Int -> a
getCReal f x n = case someNatVal $ fromIntegral n of
                    Just (SomeNat (_ :: Proxy _n)) -> let y :: ExactReal.CReal _n
                                                          y = (anyCReal x) @_n
                                                      in f y
                    Nothing -> error $ "Invalid accuracy " ++ show n
-}

--As a workaround, we can use any specific default accuracy (I chose zero), since we don't
--actually use this default. approx is implemented with atPrecision, which is n-agnostic
newtype AnyCReal = AnyCReal { crealZero :: ExactReal.CReal 0 }


atPrecision :: AnyCReal -> Int -> Integer
-- atPrecision x i = getCReal (flip ExactReal.atPrecision i) x 0
atPrecision x = ExactReal.atPrecision (crealZero x)

{-
mapCReal :: (forall n. KnownNat n => ExactReal.CReal n -> ExactReal.CReal n) -> AnyCReal -> AnyCReal
mapCReal f x = AnyCReal $ f $ anyCReal x

mapCReal2 :: (forall n. KnownNat n => ExactReal.CReal n -> ExactReal.CReal n -> ExactReal.CReal n) -> AnyCReal -> AnyCReal -> AnyCReal
mapCReal2 f x y = AnyCReal $ f (anyCReal x) (anyCReal y)
-}
mapCReal :: (ExactReal.CReal 0 -> ExactReal.CReal 0) -> AnyCReal -> AnyCReal
mapCReal f x = AnyCReal $ f $ crealZero x

mapCReal2 :: (ExactReal.CReal 0 -> ExactReal.CReal 0 -> ExactReal.CReal 0) -> AnyCReal -> AnyCReal -> AnyCReal
mapCReal2 f x y = AnyCReal $ f (crealZero x) (crealZero y)


instance CompReal AnyCReal where
    -- approx x n = getCReal toRational x n
    approx x n = atPrecision x n % pow2 n

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

roundD :: Integer -> Integer -> Integer
roundD n d = case divMod n d of
              (q, r) -> case compare (unsafeShiftL r 1) d of
                LT -> q
                _  -> q + 1

--So, because exact-real has a dumb implementation of limits, we have to do it ourselves
--really tho. it uses equality to test if the value of the limit was reached. like, bruhh
instance Limit Rational AnyCReal where
    limit f = AnyCReal $ ExactReal.crMemoize (\i -> let (a :% b) = f (i+1) in roundD (shiftL a i) b)

instance Limit AnyCReal AnyCReal where
    limit f = AnyCReal $ ExactReal.crMemoize (\i -> let x = f (i+1) in (atPrecision x (i+1) + 1) `div` 2)
    errorLimit = errorLimitDef

instance CompOrd AnyCReal where
    domCompare = domCompareDef (\x n -> let m = atPrecision x n in (m-1, m+1))
    compMin = mapCReal2 min
    compMax = mapCReal2 max

instance Powers AnyCReal where
    --The default implementation is faster than this definition
    -- pow = powDef atPrecision (\f -> AnyCReal $ ExactReal.crMemoize f) scale
    --     where scale x n | n >= 0    = shift x n
    --                     | otherwise = shift (x + bit (-n-1)) n

instance Boundable AnyCReal


instance Show AnyCReal where
    --show x = getCReal show x 80
    show = ExactReal.showAtPrecision 80 . crealZero