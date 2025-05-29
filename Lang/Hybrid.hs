module Lang.Hybrid where

import CompOrd

import Control.Monad (ap, liftM2)
import Data.Maybe

--plugging in a t less than 0 or greater than the duration should be invalid
--endpoint is memo'ed to avoid unnecessary time comparisons
newtype Hybrid t a = Hybrid (t -> a, Maybe (t, a)) deriving (Functor)

type HProgram t a = a -> Hybrid t a


wait :: t -> HProgram t a
wait t x = Hybrid (const x, Just (t, x))

end :: HProgram t a
end x = Hybrid (const x, Nothing)

for :: (t -> a) -> t -> Hybrid t a
for f d = Hybrid (f, Just (d, f d))

forever :: (t -> a) -> Hybrid t a
forever f = Hybrid (f, Nothing)


eval :: Hybrid t a -> t -> a
eval (Hybrid (f, _)) = f --TODO: validate time?

duration :: Hybrid t a -> Maybe t
duration (Hybrid (_, m)) = fmap fst m

startpoint :: (Num t) => Hybrid t a -> a
startpoint (Hybrid (f, _)) = f 0

endpoint :: Hybrid t a -> a
endpoint (Hybrid (_, m)) = maybe (error "endpoint of infinite hybrid program") snd m

mEndpoint :: Hybrid t a -> Maybe a
mEndpoint (Hybrid (_, m)) = fmap snd m


join :: (Num t, Ord t) => Hybrid t a -> Hybrid t a -> Hybrid t a
join x y = Hybrid (maybe (eval x) h (duration x), liftM2 joinDur (dur x) (dur y))
    where dur (Hybrid (_, m)) = m
          joinDur (t1, _) (t2, e) = (t1 + t2, e)
          h d t | t < d       = eval x t
                | otherwise   = eval y (t - d)

joinComp :: (Num t, CompOrd t) => Int -> Hybrid t a -> Hybrid t a -> Hybrid t (Maybe a)
joinComp n x y = Hybrid (maybe (Just . eval x) h (duration x), liftM2 joinDur (dur x) (dur y))
    where dur (Hybrid (_, m)) = m
          joinDur (t1, _) (t2, e) = (t1 + t2, Just e)
          h d t = case mCompare t d n of
                    Nothing -> Nothing
                    Just LT -> Just $ eval x t
                    _       -> Just $ eval y (t - d)


compose :: (Ord t, Num t) => HProgram t a -> HProgram t a -> HProgram t a
compose f g x = let h = f x in join h (g $ endpoint h)

composeComp :: (Num t, CompOrd t) => Int -> HProgram t a -> HProgram t a -> a -> Hybrid t (Maybe a)
composeComp n f g x = let h = f x in joinComp n h (g $ endpoint h)


instance (Num t, Ord t) => Applicative (Hybrid t) where
    pure  = wait 0
    (<*>) = ap

instance (Num t, Ord t) => Monad (Hybrid t) where
    h >>= f = join (fmap (startpoint . f) h) (endpoint $ fmap f h) --can this be optimized?