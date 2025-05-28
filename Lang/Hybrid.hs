module Lang.Hybrid where

import Control.Monad (ap, liftM2)
import Data.Maybe

--plugging in a t less than 0 or greater than the duration should be invalid
--endpoint is memo'ed to avoid time comparisons
newtype Hybrid t a = Hybrid (t -> a, Maybe t, a) deriving (Functor)

type HProgram t a = a -> Hybrid t a


wait :: t -> HProgram t a
wait t x = Hybrid (const x, Just t, x)

end :: HProgram t a
end x = Hybrid (const x, Nothing, error "endpoint of infinite hybrid program")

for :: (t -> a) -> t -> Hybrid t a
for f d = Hybrid (f, Just d, f d)

forever :: (t -> a) -> Hybrid t a
forever f = Hybrid (f, Nothing, error "endpoint of infinite hybrid program")


eval :: Hybrid t a -> t -> a
eval (Hybrid (f, _, _)) = f --TODO: validate time?

duration :: Hybrid t a -> Maybe t
duration (Hybrid (_, d, _)) = d

startpoint :: (Num t) => Hybrid t a -> a
startpoint (Hybrid (f, _, _)) = f 0

endpoint :: Hybrid t a -> a
endpoint (Hybrid (_, _, e)) = e

--TODO: use CompOrd
join :: (Ord t, Num t) => Hybrid t a -> Hybrid t a -> Hybrid t a
join x y = Hybrid (maybe (eval x) h (duration x), liftM2 (+) (duration x) (duration y), endpoint y)
    where h d t | t < d       = eval x t
                | otherwise   = eval y (t - d)

compose :: (Ord t, Num t) => HProgram t a -> HProgram t a -> HProgram t a
compose f g x = let h = f x in join h (g $ endpoint h)


instance (Ord t, Num t) => Applicative (Hybrid t) where
    pure  = wait 0
    (<*>) = ap

instance (Ord t, Num t) => Monad (Hybrid t) where
    h >>= f = join (fmap (startpoint . f) h) (endpoint $ fmap f h) --can this be optimized?