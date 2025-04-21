--This module was heavily inspired by https://github.com/sydow/ireal/blob/master/applications/FAD.hs
module Deriv where

import Data.List (null, transpose)
import GHC.TypeNats (Nat)
import Control.Applicative (ZipList)


--This class was taken and adapted from https://github.com/sydow/ireal/blob/master/Data/Number/IReal/Powers.hs

-- | Common functions collected to allow for instances which
-- handle dependency problems for intervals, and for automatic
-- differentiation.
class Num a => Powers a where
   -- squaring function; in @sq x@, there is only one occurrence of @x@ (as opposed to @x * x@)
   sq :: a -> a
   -- power function; @pow n x@ computes @x^n@, but can be implemented for 'IReal' with correct
   -- treatment of dependency.
   pow :: a -> Int -> a
   -- list of powers; a more efficient alternative to @pow@ when all powers of @x@ are used
   powers :: a -> [a]
   sq = flip pow 2
   pow x n = x ^ n
   powers x = iterate (*x) 1

instance Powers Double 
instance Powers Integer

{-
data Deriv a (n :: Nat) where
    DNil  :: Deriv a n
    DZero :: a -> Deriv a 0
    DCons :: Deriv a n -> Deriv a (n+1) -> Deriv a (n+1)
-}

--because we have no type information about the dimensionality of a Deriv, a non-nill Deriv must be
--a fixed point in the dimensionality, that is, an infinite recursion. This means we need to store
--the value @a at every level, and memoize it in every operation to prevent wasteful computations
data Deriv a = DNil | DCons a (Deriv a) (Deriv a)
--                               deg       ind
--     n        n                 n        n-1

con :: a -> Deriv a
con x = DCons x DNil (con x)

var :: (Num a) => Nat -> a -> Deriv a
var 0 x = DCons x (con 1) (con x)
var i x = DCons x DNil (var (i-1) x)

val :: Deriv a -> a
val (DCons x _ _) = x

dvx :: Deriv a -> Deriv a
dvx DNil = DNil
dvx (DCons _ dv _) = dv

dvr :: Deriv a -> Deriv a
dvr DNil = DNil
dvr (DCons _ _ dv) = dv

nil :: Deriv a -> Bool
nil DNil = True
nil _    = False

numVal :: (Num a) => Deriv a -> a
numVal DNil = 0
numVal (DCons x _ _) = x


--the functor instance includes memoization on the redundant fields to speed up computations
instance Functor Deriv where
    fmap f DNil = DNil
    fmap f dv@(DCons x _ _) = mem f (f x) dv
        where mem f y DNil = DNil
              mem f y (DCons _ dv1 dv2) = DCons y (fmap f dv1) (mem f y dv2)

--same with zip
zipDeriv :: (a -> b -> c) -> Deriv a -> Deriv b -> Deriv c
zipDeriv f dv1 dv2 = mem f (f (val dv1) (val dv2)) dv1 dv2
    where mem f z DNil _ = DNil
          mem f z _ DNil = DNil
          mem f z (DCons x dv1 dv2) (DCons y dv3 dv4) = DCons z (zipDeriv f dv1 dv3) (mem f z dv2 dv4)



--given the index of a variable, returns the partial derivative over that variable
deriv :: (Num a) => Nat -> Deriv a -> a
deriv _ DNil = 0
deriv 0 (DCons _ dv _) = numVal dv
deriv i (DCons _ _ dv) = deriv (i-1) dv

--given the index of a variable and a natural n, returns the degree n partial derivative over that variable
derivDeg :: (Num a) => Nat -> Nat -> Deriv a -> a
derivDeg _ _ DNil = 0
derivDeg i 0 (DCons x _ _)  = x
derivDeg 0 d (DCons _ dv _) = derivDeg 0 (d-1) dv
derivDeg i d (DCons _ _ dv) = derivDeg (i-1) d dv

--given a degree for each variable (as a list), returns the correspondent partial derivative
vecDeriv :: (Num a) => [Nat] -> Deriv a -> a
vecDeriv _ DNil = 0
vecDeriv [] (DCons x _ _) = x
vecDeriv (0:ds) (DCons _ _ dv) = vecDeriv ds dv
vecDeriv (d:ds) (DCons _ dv _) = vecDeriv (d-1 : ds) dv

fullVecDeriv :: (Num a) => [Nat] -> Deriv a -> Deriv a
fullVecDeriv _ DNil = DNil
fullVecDeriv [] dv = dv
fullVecDeriv (0:ds) (DCons _ dv1 dv2) = case rec of
                                            DNil -> DNil
                                            DCons x _ _ -> DCons x (fullVecDeriv (0:ds) dv1) rec
    where rec = fullVecDeriv ds dv2
fullVecDeriv (d:ds) (DCons _ dv _) = fullVecDeriv (d-1 : ds) dv


--TODO: fix memOp everywhere
--applies a function and memoizes the val of the result to prevent duplicated computations in the redundant fields
memOp :: (Deriv a -> Deriv b) -> b -> Deriv a -> Deriv b
memOp f y DNil = DNil
memOp f y (DCons _ dv1 dv2) = DCons y (f dv1) (memOp f y dv2) --TODO: generalize? currently only works for map-like operations

--applies a binary operation and memoizes the val of the result to prevent duplicated computations in the redundant fields
memOp2 :: (Deriv a -> Deriv b -> Deriv c) -> c -> Deriv a -> Deriv b -> Deriv c
memOp2 f z DNil DNil = DNil
memOp2 f z (DCons _ dv1 dv2) DNil = DCons z (f dv1 DNil) (memOp2 f z dv2 DNil)
memOp2 f z DNil (DCons _ dv1 dv2) = DCons z (f DNil dv1) (memOp2 f z DNil dv2)
memOp2 f z (DCons _ dv1 dv2) (DCons _ dv3 dv4) = DCons z (f dv1 dv3) (memOp2 f z dv2 dv4)

--f' must accept any dimensionality (number of variables) of Deriv
chain :: (Num a) => (a -> a) -> (Deriv a -> Deriv a) -> Deriv a -> Deriv a
chain f f' DNil = DNil --TODO: is this case right? what about val? (check other chains too)
chain f f' g@(DCons x dv1 dv2) = DCons (f x) (dv1 * f' g) (chain f f' dv2) --TODO: mem
--chain f f' g = memOp aux (f x) g
--   where aux (DCons x dv1 dv2) = 


--f' must accept any dimensionality (number of variables) of Deriv
rchain :: (Num a) => (a -> a) -> (Deriv a -> Deriv a) -> Deriv a -> Deriv a
rchain f f' DNil = DNil
rchain f f' (DCons x dv1 dv2) = dv
    where y = f x
          dv = DCons y (dv1 * f' dv) (memOp (rchain f f') y dv2)

r2chain :: (Num a) => (a -> a) -> (a -> a) -> (Deriv a -> Deriv a) -> (Deriv a -> Deriv a) -> Deriv a -> Deriv a
r2chain f1 f2 f1' f2' DNil = DNil
r2chain f1 f2 f1' f2' dv = fst (mutRec dv)
    where y1 = f1 (numVal dv)
          y2 = f2 (numVal dv)
          mutRec (DCons _ dv1 dv2) = (z, w)
            where z = DCons y1 (dv1 * f1' w) zr
                  w = DCons y2 (dv1 * f2' z) wr
                  (zr, wr) = mutRec dv2

mchain :: (Num b) => (Deriv a -> Deriv b -> Deriv b) -> ([a] -> b) -> ([Deriv a] -> [Deriv b]) -> [Deriv a] -> Deriv b
mchain mult f f' gs = DCons (f $ map val gs) (sum $ zipWith mult (map dvx gs) (f' gs)) (mchain mult f f' (map dvr gs)) --TODO: mem

--  a = [r]
--  b = r
gchain :: (Deriv a -> Deriv b -> Deriv b) -> (a -> b) -> (Deriv a -> Deriv b) -> Deriv a -> Deriv b
gchain mult f f' DNil = DNil
gchain mult f f' g@(DCons xs dv1 dv2) = DCons (f xs) (mult dv1 (f' g)) (gchain mult f f' dv2) --TODO: mem

--TODO: refactor this
data Grid a = Grid a [a] [a] (Grid a) deriving (Functor)

current :: Grid a -> a
current (Grid x _ _ _) = x

right :: Grid a -> Grid a
right (Grid _ (a:bs) _ d@(Grid c _ cs _)) = Grid a bs (c:cs) (right d)

down :: (Grid a -> Grid a)
down (Grid _ _ (a:cs) d@(Grid b bs _ _)) = Grid a (b:bs) cs (down d)

pascalGrid :: Grid Integer
pascalGrid = aux 1 (repeat 1) (repeat 1)
    where rollingSum x ys = tail $ scanl (+) x ys
          aux a (b:bs) (c:cs) = Grid a (b:bs) (c:cs) $ aux a1 (rollingSum a1 bs) (rollingSum a1 cs)
            where a1 = b + c

instance (Num a) => Num (Deriv a) where
    DNil + dv = dv
    dv + DNil = dv
    dv1 + dv2 = memOp2 (+) (val dv1 + val dv2) dv1 dv2

    DNil - dv = negate dv
    dv - DNil = dv
    dv1 - dv2 = memOp2 (-) (val dv1 - val dv2) dv1 dv2

    DNil * dv = DNil
    dv * DNil = DNil
    --a@(DCons x dv1 dv2) * b@(DCons y dv3 dv4) = DCons (x * y) (dv1*b + a*dv3) (dv2 * dv4)
    dv1 * dv2 = (sum . map (\(a,b,c,_) -> val a * val b * current c)) <$> aux [(dv1, dv2, fromInteger <$> pascalGrid, True)]
        where aux [] = DNil
              aux l  = DCons l (aux (l >>= step)) (aux $ map (\(a,b,c,d) -> (dvr a, dvr b, c, d)) l)
              step (a, b, c, True)  = filter valid [(dvx a, b, down c, True), (a, dvx b, right c, False)]
              step (a, b, c, False) = filter valid [(a, dvx b, right c, False)]
              valid (a, b, _, _)    = not (nil a) && not (nil b)

    --negate = chain negate (const $ con $ -1)
    negate = fmap negate

    --abs (DCons x dv1 dv2) | x < 0 = DCons (negate x) (negate dv1) (negate dv2)
    --                      | otherwise = DCons x dv1 dv2
    abs = chain abs signum --TODO: does this work for complex numbers??

    --signum = chain signum (const DNil)
    signum dv = memOp (const DNil) (signum $ val dv) dv

    fromInteger = con . fromInteger


instance (Fractional a, Powers a) => Fractional (Deriv a) where
    recip = rchain recip (negate . sq)
    fromRational = con . fromRational

instance (Floating a, Powers a) => Floating (Deriv a) where 
    pi       = con pi
    exp      = rchain exp id
    log      = chain log recip
    sqrt     = rchain sqrt (recip . (2*))
    sin      = r2chain sin cos id negate
    cos      = r2chain cos sin negate id
    tan      = rchain tan ((1+) . sq)
    asin     = chain asin (recip . sqrt . (1-) . sq)
    acos     = chain acos (negate . recip . sqrt . (1-) . sq)
    atan     = chain atan (recip . (1+) . sq)
    sinh     = r2chain sinh cosh id id
    cosh     = r2chain cosh sinh id id
    asinh    = chain asinh (recip . sqrt . (1+) . sq)
    acosh    = chain acosh (recip . sqrt . (\x -> x-1) . sq)
    atanh    = chain atanh (recip . (1-) . sq)

instance (Num a, Powers a) => Powers (Deriv a) where
    sq       = chain sq (2*)
    pow x 0  = con 1
    pow x n  = chain (flip pow n) ((fromIntegral n *) . flip pow (n-1)) x 
    -- Note: This is linear in n, but behaves correctly on intervals





instance (Show a) => Show (Deriv a) where
    show = showLim 3
        where showLim n DNil = "DNil"
              showLim 0 _ = "..."
              showLim n (DCons x dv1 dv2) = "(" ++ show x ++ " {" ++ showLim (n-1) dv2 ++ "}) " ++ showLim (n-1) dv1

instance (Num b) => Num (a -> b) where
    f + g = \x -> f x + g x
    f - g = \x -> f x - g x
    f * g = \x -> f x * g x
    negate = (negate .)
    abs = (abs .)
    signum = (signum .)
    fromInteger = const . fromInteger

instance (Fractional b) => Fractional (a -> b) where
    f / g = \x -> f x / g x
    recip = (recip .)
    fromRational = const . fromRational

instance (Floating b) => Floating (a -> b) where
    pi       = const pi
    exp      = (exp .)
    log      = (log .)
    sqrt     = (sqrt .)
    sin      = (sin .)
    cos      = (cos .)
    tan      = (tan .)
    asin     = (asin .)
    acos     = (acos .)
    atan     = (atan .)
    sinh     = (sinh .)
    cosh     = (cosh .)
    asinh    = (asinh .)
    acosh    = (acosh .)
    atanh    = (atanh .)

instance (Powers b) => Powers (a -> b) where
    pow f n = \x -> pow (f x) n


--Could be generalized to any Applicative, but we would have to use ZipList
instance (Num a) => Num [a] where
    (+) = zipWith (+)
    (-) = zipWith (-)
    (*) = zipWith (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = repeat . fromInteger

instance (Fractional a) => Fractional [a] where
    (/) = zipWith (/)
    recip = fmap recip
    fromRational = repeat . fromRational

instance (Floating a) => Floating [a] where
    pi       = repeat pi
    exp      = fmap exp
    log      = fmap log
    sqrt     = fmap sqrt
    sin      = fmap sin
    cos      = fmap cos
    tan      = fmap tan
    asin     = fmap asin
    acos     = fmap acos
    atan     = fmap atan
    sinh     = fmap sinh
    cosh     = fmap cosh
    asinh    = fmap asinh
    acosh    = fmap acosh
    atanh    = fmap atanh

instance (Powers a) => Powers [a] where
    pow xs n = map (flip pow n) xs
    powers = transpose . map powers


--x,y,z :: Deriv (Double -> Double -> Double -> Double)
--x = var 0 (\a b c -> a)
--y = var 1 (\a b c -> b)
--z = var 2 (\a b c -> c)

--x :: Deriv (Double -> Double)
--x = var 0 id

{-
test1 = x + x + x - y - y + z
test2 = x * y + z * sin x + pow y 2
test3 = pow x 2 * exp (y - tan z) / (sin y + cos x)
-}