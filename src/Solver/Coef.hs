{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Solver.Coef (Coef, numCoef, evalCoef, coefRealPow) where

import Utils
import CompOrd
import Powers
import Boundable

import GHC.Real (Ratio(..))
import Control.Monad (ap)

--coefficients are the product of an integer part and a real part for efficency
newtype Coef r = Coef { unCoef :: (Rational, Maybe r) } deriving (Functor)

numCoef :: r -> Coef r
numCoef r = Coef (1, Just r)

evalCoef :: (Fractional r) => Coef r -> r
evalCoef (Coef (n, Nothing)) = fromRational n
evalCoef (Coef (0, Just _)) = 0
evalCoef (Coef (1, Just c)) = c
evalCoef (Coef (-1, Just c)) = negate c
evalCoef (Coef (n, Just c)) = fromRational n * c

coefAux :: (Fractional a) => (Rational -> b) -> (a -> b) -> Coef a -> b
coefAux f _ (Coef (n, Nothing)) = f n
coefAux f _ (Coef (0, _)) = f 0
coefAux _ g x = g (evalCoef x)

coef2Aux :: (Fractional a, Fractional b) => (Rational -> Rational -> c) -> (a -> b -> c) -> Coef a -> Coef b -> c
coef2Aux f _ (Coef (n, Nothing)) (Coef (m, Nothing)) = f n m
coef2Aux f _ (Coef (0, _)) (Coef (0, _)) = f 0 0
coef2Aux f _ (Coef (n, Nothing)) (Coef (0, _)) = f n 0
coef2Aux f _ (Coef (0, _)) (Coef (m, Nothing)) = f 0 m
coef2Aux _ g x y = g (evalCoef x) (evalCoef y)

instance (Fractional r, Show r) => Show (Coef r) where
    show (Coef (0, _)) = "0"
    show (Coef (1, Nothing)) = "1"
    show (Coef (-1, Nothing)) = "-1"
    show (Coef (n, Nothing)) = "(" ++ show n ++ ")"
    show (Coef (n, Just c)) = show (fromRational n * c)

instance (Fractional r, Eq r) => Eq (Coef r) where
    (==) = coef2Aux (==) (==)

instance (Fractional r, Ord r) => Ord (Coef r) where
    compare = coef2Aux compare compare
    max = coef2Aux (fromRational .-. max) (numCoef .-. max)
    min = coef2Aux (fromRational .-. min) (numCoef .-. min)

instance (Fractional r, CompOrd r) => CompOrd (Coef r) where
    domCompare = coef2Aux (const .-. Top .-. compare) domCompare
    infCompare = coef2Aux ((Nothing,) .-. compare) infCompare
    compMax = coef2Aux (Coef .-. (,Nothing) .-. max) (numCoef .-. compMax)
    compMin = coef2Aux (Coef .-. (,Nothing) .-. min) (numCoef .-. compMin)

instance (Fractional r) => Num (Coef r) where
    (+) = coef2Aux (fromRational .-. (+)) (numCoef .-. (+))
    (-) = coef2Aux (fromRational .-. (-)) (numCoef .-. (-))
    (Coef (0, _)) * _ = 0
    _ * (Coef (0, _)) = 0
    (Coef (n, mx)) * (Coef (m, my)) = Coef (n * m, maybe my (\x -> Just $ maybe x (x*) my) mx)
    negate = Coef . (negate >< id) . unCoef
    abs = Coef . (abs >< fmap abs) . unCoef
    signum (Coef (n, mx)) = case signum n of
                            0  -> 0
                            s  -> Coef (s, fmap signum mx)
    fromInteger n = Coef (fromInteger n, Nothing)

instance (Fractional r) => Fractional (Coef r) where
    (Coef (0, _)) / _ = 0
    _ / (Coef (0, _)) = error "divide by 0"
    (Coef (n, mx)) / (Coef (m, my)) = Coef (n / m, maybe (fmap recip my) (\x -> Just $ maybe x (x/) my) mx)
    recip (Coef (n, mx)) = Coef (recip n, fmap recip mx)
    fromRational n = Coef (n, Nothing)

instance (Fractional r, Powers r) => Powers (Coef r) where
    pow _ 0 = 1
    pow x 1 = x
    pow x n = coefAux (fromRational . (^^n)) (numCoef . (`pow` n)) x


instance (Fractional r, Boundable r) => Boundable (Coef r) where
    bounds (Coef (0, _))       = (0, 0)
    bounds (Coef (n, Nothing)) = bounds n
    bounds c                   = bounds $ evalCoef c

instance Applicative Coef where
    pure = numCoef
    (<*>) = ap

instance Monad Coef where
    (Coef (n, Nothing)) >>= _ = Coef (n, Nothing)
    (Coef (n, Just x))  >>= f = Coef $ ((n*) >< id) $ unCoef $ f x


coefRealPow :: (Floating r, Powers r) => Coef r -> Coef r -> Coef r
coefRealPow (Coef (0, _)) (Coef (0, _)) = error "0^0"
coefRealPow (Coef (0, _)) _ = 0
coefRealPow _ (Coef (0, _)) = 1
coefRealPow x (Coef (m :% 1, Nothing)) = pow x (fromInteger m)
coefRealPow x y = numCoef $ evalCoef x ** evalCoef y