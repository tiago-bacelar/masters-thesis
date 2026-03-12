{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CompReal.Instances.ERA (ERA.CReal) where

import Utils
import CompOrd
import CompReal
import Boundable
import Powers
import Limit

--ERA is available in numbers as Data.Numbers.CReal, but
--that version doesn't export CR, making it kinda useless
import qualified CompReal.Instances.ERA.CReal as ERA

import GHC.Real (Ratio(..), (%))
import Data.Bits

unCR :: ERA.CReal -> Int -> Integer
unCR (ERA.CR x') = x'

instance CompReal ERA.CReal where
    approx x n = unCR x n % pow2 n

instance Limit Rational ERA.CReal where
    limit f = ERA.CR (\i -> let (a :% b) = f (i+1) in ERA.round_uk (shiftL a i % b))

instance Limit ERA.CReal ERA.CReal where
    limit f = ERA.CR (\i -> (unCR (f (i+1)) (i+1) + 1) `div` 2)
    errorLimit = errorLimitDef

instance CompOrd ERA.CReal where
    domCompare = domCompareDef (\x n -> let m = unCR x n in (m-1, m+1))
    compMin = min
    compMax = max

instance Powers ERA.CReal where
    pow = powDef unCR ERA.CR scale
        where scale x n | n >= 0    = shift x n
                        | otherwise = shift (x + bit (-n-1)) n

instance Boundable ERA.CReal