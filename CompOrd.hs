{-# LANGUAGE DefaultSignatures #-}

module CompOrd where

import Utils

import Data.Maybe
import Debug.Trace


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
    compMax :: a -> a -> a
    compMin :: a -> a -> a

    default domCompare :: (Ord a) => a -> a -> Int -> OrderingDomain
    domCompare x y _ = Top (compare x y)
    infCompare x y = head $ catMaybes $ map (asTop . domCompare x y) [0..]
    x <! y = extendedBy (Top LT) . domCompare x y
    x >! y = extendedBy (Top GT) . domCompare x y
    compMax x y = case infCompare x y of
                    LT -> y
                    _  -> x
    compMin x y = case infCompare x y of
                    LT -> x
                    _  -> y

instance CompOrd Double
instance CompOrd Integer


--a default implementation of compMax using a Fractional constraint
compMaxDef :: (Fractional a) => a -> a -> a
compMaxDef x y = (x + y + abs (x - y)) / 2

--a default implementation of compMin using a Fractional constraint
compMinDef :: (Fractional a) => a -> a -> a
compMinDef x y = (x + y - abs (x - y)) / 2



compMaximum :: (CompOrd a) => [a] -> a
compMaximum = foldTree1 compMax

compMinimum :: (CompOrd a) => [a] -> a
compMinimum = foldTree1 compMin