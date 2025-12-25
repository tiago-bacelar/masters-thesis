{-# LANGUAGE DeriveFunctor #-}

module Lang.Hybrid (
    Hybrid(..),
    smap,
    fsmap,
    wait,
    end,
    for,
    forever,
    instant,
    eval,
    duration,
    endpoint,
    takeH,
    dropH,
    joinH,
    Query(..),
    CompHybrid(..),
    smapCH,
    fsmapCH,
    instantCH,
    evalCH,
    unrollCH,
    durationCH,
    endpointCH,
    joinCH,
    unCH) where

import Utils
import CompOrd

import Data.List (singleton)
import Control.Monad (ap, join)

--the function eval must be defined in [0, d), where d is the duration
data Hybrid t s a = Hybrid { eval :: t -> s, unroll :: Maybe (t, a) } deriving (Functor)

smap :: (r -> s) -> Hybrid t r a -> Hybrid t s a
smap f (Hybrid e m) = Hybrid (f . e) m

fsmap :: (r -> s) -> Hybrid t r r -> Hybrid t s s
fsmap f = fmap f . smap f


wait :: t -> s -> Hybrid t s s
wait t x = Hybrid (const x) (Just (t, x))

end :: s -> Hybrid t s s
end x = Hybrid (const x) Nothing

for :: (t -> s) -> t -> Hybrid t s s
for f d = Hybrid f (Just (d, f d))

forever :: (t -> s) -> Hybrid t s a
forever f = Hybrid f Nothing

instant :: (Num t) => a -> Hybrid t s a
instant x = Hybrid (const undefined) (Just (0, x)) --TODO: is this undefined ok? i feel like it isnt


duration :: Hybrid t s a -> Maybe t
duration = fmap fst . unroll

endpoint :: Hybrid t s a -> Maybe a
endpoint = fmap snd . unroll


takeH :: (Ord t) => t -> Hybrid t s s -> Hybrid t s s
takeH t (Hybrid f m) = Hybrid f (aux m)
    --aux is factored out to delay evaluating the Maybe as long as possible
    where aux (Just (d, x)) = Just (min t d, if t < d then f t else x)
          aux Nothing       = Just (t, f t)

--assumes t <= duration h
dropH :: (Num t) => t -> Hybrid t s a -> Hybrid t s a
dropH t (Hybrid f m) = Hybrid (f . (t+)) (fmap (\(d,x) -> (d-t,x)) m)

joinH :: (Num t, Ord t) => Hybrid t s a -> Hybrid t s b -> Hybrid t s b
joinH (Hybrid f Nothing) _                  = Hybrid f Nothing
joinH (Hybrid f (Just (d, _))) (Hybrid g m) = Hybrid h (fmap ((d+) >< id) m)
    where h t | t < d       = f t
              | otherwise   = g (t - d)


instance (Num t, Ord t) => Applicative (Hybrid t s) where
    pure  = instant
    (<*>) = ap

instance (Num t, Ord t) => Monad (Hybrid t s) where
    h@(Hybrid _ (Just (_,x)))   >>= f = joinH h (f x)
    (Hybrid e Nothing)          >>= f = Hybrid e Nothing




newtype Query s = Query { runQuery :: Maybe Int -> [s] } deriving (Functor)

instance Applicative Query where
    pure  = Query . const . singleton
    (<*>) = ap

instance Monad Query where
    q >>= f = Query $ \n -> runQuery q n >>= (($n) . runQuery . f)


--unline Hybrid, the function inside Hyb only needs to be
--defined in (0,d) (or (0,inf) if duration is infinite)
data CompHybrid t s a = Ins a | Hyb (Hybrid t (Query s) a) deriving (Functor)

smapCH :: (r -> s) -> CompHybrid t r a -> CompHybrid t s a
smapCH f (Ins x) = Ins x
smapCH f (Hyb h) = Hyb $ smap (fmap f) h

fsmapCH :: (r -> s) -> CompHybrid t r r -> CompHybrid t s s
fsmapCH f = fmap f . smapCH f

instantCH :: a -> CompHybrid t s a
instantCH = Ins

evalCH :: CompHybrid t s a -> t -> Query s
evalCH (Hyb h) = eval h

unrollCH :: (Num t) => CompHybrid t s a -> Maybe (t, a)
unrollCH (Ins x) = Just (0, x)
unrollCH (Hyb h) = unroll h

durationCH :: (Num t) => CompHybrid t s a -> Maybe t
durationCH (Ins x) = Just 0
durationCH (Hyb h) = duration h

endpointCH :: CompHybrid t s a -> Maybe a
endpointCH (Ins x) = Just x
endpointCH (Hyb h) = endpoint h

joinCH :: (Num t, CompOrd t) => Hybrid t (Query s) a -> Hybrid t (Query s) b -> Hybrid t (Query s) b
joinCH (Hybrid f Nothing) _                     = Hybrid f Nothing
joinCH (Hybrid f (Just (d, _))) (Hybrid g m)    = Hybrid (join . Query . h) (fmap ((d+) >< id) m)
    where h t Nothing  = if infCompare t d == LT then [f t] else [g (t - d)]
          h t (Just n) = case mCompare (Top LT) (domCompare t d n) of
                            Just True  -> [f t]             --The wrong branch is evaluated
                            Just False -> [g (t - d)]       --outside its original domain.
                            Nothing    -> [f t, g (t - d)]  --Potentially dangerous

unCH :: (Num t) => CompHybrid t s a -> Hybrid t (Query s) a
unCH (Ins x) = instant x
unCH (Hyb h) = h


instance (Num t, CompOrd t) => Applicative (CompHybrid t s) where
    pure  = Ins
    (<*>) = ap

instance (Num t, CompOrd t) => Monad (CompHybrid t s) where
    (Ins x)                         >>= f = f x
    (Hyb h@(Hybrid e Nothing))      >>= f = Hyb $ Hybrid e Nothing
    (Hyb h@(Hybrid e (Just (d,x)))) >>= f
        = Hyb $ case f x of
                Ins y -> Hybrid e (Just (d,y))
                Hyb i -> joinCH h i
