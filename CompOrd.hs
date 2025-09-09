{-# LANGUAGE DefaultSignatures #-}

module CompOrd where

import Data.Maybe


data OrderingDomain = Bottom | LEQ | NEQ | GEQ | Top Ordering deriving (Eq)

asTop :: OrderingDomain -> Maybe Ordering
asTop (Top o) = Just o
asTop _ = Nothing

--complete partial order (cpo)
extendedBy :: OrderingDomain -> OrderingDomain -> Bool
extendedBy Bottom _ = True
extendedBy LEQ (Top LT) = True
extendedBy LEQ (Top EQ) = True
extendedBy NEQ (Top LT) = True
extendedBy NEQ (Top GT) = True
extendedBy GEQ (Top EQ) = True
extendedBy GEQ (Top GT) = True
extendedBy x y | x == y = True
               | otherwise = False

consistent :: OrderingDomain -> OrderingDomain -> Bool
consistent x y = x `extendedBy` y || y `extendedBy` x

--a continuous function (in the domain theory sense) that compares two elements of OrderingDomain
mCompare :: OrderingDomain -> OrderingDomain -> Maybe Bool
mCompare x y | not (consistent x y) = Just False
             | x `extendedBy` y = Just True
             | otherwise = Nothing


class CompOrd a where
    --the following must be true: extendedBy (domCompare x y n) (domCompare x y (n+1))
    domCompare :: a -> a -> Int -> OrderingDomain
    infCompare :: a -> a -> Ordering
    (<!) :: a -> a -> Int -> Bool
    (>!) :: a -> a -> Int -> Bool

    default domCompare :: (Ord a) => a -> a -> Int -> OrderingDomain
    domCompare x y _ = Top (compare x y)
    infCompare x y = head $ catMaybes $ map (asTop . domCompare x y) [0..]
    x <! y = extendedBy (Top LT) . domCompare x y
    x >! y = extendedBy (Top GT) . domCompare x y

instance CompOrd Double
instance CompOrd Integer