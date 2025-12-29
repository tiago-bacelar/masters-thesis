{-# LANGUAGE DefaultSignatures #-}

module CompOrd (
    OrderingDomain(..),
    MidOrdering(..),
    asTop,
    extendedBy,
    consistent,
    mCompare,
    CompOrd(..),
    domCompareAux,
    infCompareAux,
    compMinDef,
    compMaxDef,
    lesserInf,
    greaterInf,
    equalInf,
    lesserEqInf,
    greaterEqInf,
    differentInf,
    compMinimum,
    compMaximum) where

import Utils

import Data.List (uncons)
import Data.Maybe (catMaybes)


data MidOrdering = LEQ | NEQ | GEQ deriving (Eq)
data OrderingDomain = Bottom | Middle MidOrdering | Top Ordering deriving (Eq)

asTop :: OrderingDomain -> Maybe Ordering
asTop (Top o) = Just o
asTop _ = Nothing

--complete partial order (cpo)
extendedBy :: OrderingDomain -> OrderingDomain -> Bool
extendedBy Bottom _ = True
extendedBy (Middle LEQ) (Top LT) = True
extendedBy (Middle LEQ) (Top EQ) = True
extendedBy (Middle NEQ) (Top LT) = True
extendedBy (Middle NEQ) (Top GT) = True
extendedBy (Middle GEQ) (Top EQ) = True
extendedBy (Middle GEQ) (Top GT) = True
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
    --the Maybe MidOrdering is lazier than the Ordering, and the two must be consistent
    infCompare :: a -> a -> (Maybe MidOrdering, Ordering)
    (<!) :: a -> a -> Int -> Bool
    (>!) :: a -> a -> Int -> Bool
    compMin :: a -> a -> a
    compMax :: a -> a -> a

    default domCompare :: (Ord a) => a -> a -> Int -> OrderingDomain
    domCompare x y _ = Top (compare x y)
    infCompare x y = infCompareAux $ domCompare x y <$> [0..]
    x <! y = extendedBy (Top LT) . domCompare x y
    x >! y = extendedBy (Top GT) . domCompare x y
    
    compMin x y = case infCompare x y of
                    (Just LEQ, _)   -> x
                    (Just GEQ, _)   -> y
                    (_, LT)         -> x
                    _               -> y
    compMax x y = case infCompare x y of
                    (Just LEQ, _)   -> y
                    (Just GEQ, _)   -> x
                    (_, LT)         -> y
                    _               -> x

--performs an ordering domain comparison by comparing the bounds of the two values
domCompareAux :: (Ord a) => (a, a) -> (a, a) -> OrderingDomain
domCompareAux (xl, xr) (yl, yr)
    | xl == xr && xr == yl && yl == yr  = Top EQ
    | xr == yl                          = Middle LEQ
    | xl == yr                          = Middle GEQ
    | xr < yl                           = Top LT
    | xl > yr                           = Top GT
    | otherwise                         = Bottom

infCompareAux :: [OrderingDomain] -> (Maybe MidOrdering, Ordering)
infCompareAux (Bottom : xs)     = infCompareAux xs
infCompareAux (Middle m : xs)   = (Just m, maybe (error "infCompareAux: expected infinite list") fst $ uncons $ catMaybes $ map asTop xs)
infCompareAux (Top o : _)       = (Nothing, o)
infCompareAux [] = error "infCompareAux: expected infinite list"
    

instance CompOrd Double
instance CompOrd Integer


lesserInf :: (CompOrd a) => a -> a -> Bool
lesserInf x y = case infCompare x y of
                    (Just GEQ, _)   -> False
                    (_, o)          -> o == LT

greaterInf :: (CompOrd a) => a -> a -> Bool
greaterInf x y = case infCompare x y of
                    (Just LEQ, _)   -> False
                    (_, o)          -> o == GT

equalInf :: (CompOrd a) => a -> a -> Bool
equalInf x y = case infCompare x y of
                    (Just NEQ, _)   -> False
                    (_, o)          -> o == EQ

lesserEqInf :: (CompOrd a) => a -> a -> Bool
lesserEqInf x y = case infCompare x y of
                    (Just LEQ, _)   -> True
                    (_, o)          -> o /= GT

greaterEqInf :: (CompOrd a) => a -> a -> Bool
greaterEqInf x y = case infCompare x y of
                    (Just GEQ, _)   -> True
                    (_, o)          -> o /= LT

differentInf :: (CompOrd a) => a -> a -> Bool
differentInf x y = case infCompare x y of
                    (Just NEQ, _)   -> True
                    (_, o)          -> o /= EQ

--a default implementation of compMin using a Fractional constraint
compMinDef :: (Fractional a) => a -> a -> a
compMinDef x y = (x + y - abs (x - y)) / 2

--a default implementation of compMax using a Fractional constraint
compMaxDef :: (Fractional a) => a -> a -> a
compMaxDef x y = (x + y + abs (x - y)) / 2


compMinimum :: (CompOrd a) => [a] -> a
compMinimum = foldTree1 compMin

compMaximum :: (CompOrd a) => [a] -> a
compMaximum = foldTree1 compMax