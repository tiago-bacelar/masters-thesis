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

unCR :: ERA.CReal -> Int -> Integer
unCR (ERA.CR x') = x'

{-
When looking at the source code for ERA, it is somewhat implied that each term is
the best approximation of its precision, which would mean that term i would have
an error of no more than 2^(-i-1). If that were true, we could write:
approx x n = unCR x n % pow2 n

However... when testing, I noticed that unCR pi 9 equals 1609, even though 1608/512 is
closer to the actual value of pi. So either the implementation of pi in the package is wrong,
or my assumptions about the error bound of CReal are wrong... Either way, writing approx
like I did seems to fix the problem, even though it makes every operation require a precision of
one greater than what should be theoretically necessary
-}
instance CompReal ERA.CReal where
    approx x n = unCR x (n+1) % pow2 (n+1)

--This instance of Limit is consistent with the implementation of approx, so that
--approx'ing a limit doesn't consume an extra term of the list
instance Limit Rational ERA.CReal where
    limit f = ERA.CR (\i -> ERA.round_uk (f i * toRational (pow2 i)))

instance Limit ERA.CReal ERA.CReal where
    limit f = ERA.CR (\i -> ERA.round_uk (unCR (f i) (i+1) % 2))
    errorLimit = errorLimitDef

instance CompOrd ERA.CReal where
    domCompare = domCompareDef
    compMin = min
    compMax = max

instance Powers ERA.CReal where
    pow = powDef unCR ERA.CR scale
        where scale x n | n >= 0    = shift x n
                        | otherwise = shift (x + bit (-n-1)) n

instance Boundable ERA.CReal