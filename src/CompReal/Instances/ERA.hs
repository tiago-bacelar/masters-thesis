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

import Data.Ratio
import Data.Bits

instance CompReal ERA.CReal where
    approx (ERA.CR r) n = r (n+1) % pow2 (n+1)

instance Limit Rational ERA.CReal where
    limit f = ERA.CR (\i -> ERA.round_uk (f i * toRational (pow2 i)))

instance Limit ERA.CReal ERA.CReal where
    limit f = ERA.CR (\i -> let ERA.CR g = f i in ERA.round_uk (g (i+1) % 2))
    errorLimit = errorLimitDef

instance CompOrd ERA.CReal where
    domCompare = domCompareDef
    compMin = min
    compMax = max

instance Powers ERA.CReal where
    pow = powDef (\(ERA.CR x') -> x') ERA.CR scale
        where scale x n | n >= 0    = shift x n
                        | otherwise = shift (x + bit (-n-1)) n

instance Boundable ERA.CReal