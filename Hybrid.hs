module Hybrid where

import Control.Monad (ap)


--plugging in a t less than 0 or greater than the duration should be invalid
newtype Hybrid t a = Hybrid (t -> a, t)

--it should hold that startpoint (prog x) = x
type HProgram t a = a -> Hybrid t a


eval :: Hybrid t a -> t -> a
eval (Hybrid (f, _)) = f --TODO: validate time

duration :: Hybrid t a -> t
duration (Hybrid (_, d)) = d

startpoint :: (Num t) => Hybrid t a -> a
startpoint (Hybrid (f, _)) = f 0

endpoint :: Hybrid t a -> a
endpoint (Hybrid (f, d)) = f d


compose :: (Ord t, Num t) => Hybrid t a -> Hybrid t a -> Hybrid t a
compose (Hybrid (f, d)) (Hybrid (g, e)) = Hybrid (h, d + e)
    where h t | t <= d    = f t 
              | otherwise = g (t - d)


instance Functor (Hybrid t) where
    fmap f (Hybrid (h, d)) = Hybrid (f . h, d)

instance (Ord t, Num t) => Applicative (Hybrid t) where
    pure x = Hybrid (const x, 0)
    (<*>)  = ap

instance (Ord t, Num t) => Monad (Hybrid t) where
    h >>= f = compose (fmap (startpoint . f) h) (endpoint $ fmap f h) --TODO: can this be optimized?

