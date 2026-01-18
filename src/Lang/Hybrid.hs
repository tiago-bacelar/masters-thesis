{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Lang.Hybrid (
    Hybrid,
    eval,
    unroll,
    smap,
    fsmap,
    wait,
    end,
    for,
    forever,
    instant,
    duration,
    endpoint,
    takeH,
    dropH,
    joinH,
    Query(..),
    runQueryJust,
    runQueryInf,
    CompHybrid,
    unCH,
    smapCH,
    fsmapCH,
    instantCH,
    hybridCH,
    evalCH,
    unrollCH,
    durationCH,
    endpointCH,
    CompHybridT,
    runCompHybridT) where

import Utils
import CompOrd

import Data.List.NonEmpty (NonEmpty(..))
import Data.Functor.Identity (Identity(..))
import Control.Monad (ap, join)
import Control.Monad.Trans (MonadTrans(..))

{-
The function eval assumes t in [0, d), where d is the duration.
Of course, if the duration is not known beforehand it is impossible to make safe
queries without evaluating it. Unfortunately, the duration may be _|_ (which can
happen from concatenating an infinite number of hybrids, for example), so
evaluating it is never safe. A truly safe eval would require an alternative
definition of Hybrid, which I opted not to do since it would require Ord
restrictions, which don't generalize well into CompHybrid
-}
data Hybrid t s a = Hybrid { eval :: t -> s, unroll :: Maybe (t, a) } deriving (Functor)

smap :: (r -> s) -> Hybrid t r a -> Hybrid t s a
smap f (Hybrid e m) = Hybrid (f . e) m

fsmap :: (r -> s) -> Hybrid t r r -> Hybrid t s s
fsmap f = fmap f . smap f

--Transform an eval into a safeEval
-- guard :: (Ord t) => (t -> s) -> t -> (t -> Maybe s)
-- guard f d t | t < d     = Just (f t)
--             | otherwise = Nothing


wait :: t -> s -> Hybrid t s s
wait t x = Hybrid (const x) (Just (t, x))

end :: s -> Hybrid t s s
end x = Hybrid (const x) Nothing

for :: (t -> s) -> t -> Hybrid t s s
for f d = Hybrid f (Just (d, f d))

forever :: (t -> s) -> Hybrid t s a
forever f = Hybrid f Nothing

instant :: (Num t) => a -> Hybrid t s a
instant x = Hybrid (const undefined) (Just (0, x))


duration :: Hybrid t s a -> Maybe t
duration = fmap fst . unroll

endpoint :: Hybrid t s a -> Maybe a
endpoint = fmap snd . unroll


--assumes t >= 0
takeH :: (Ord t) => t -> Hybrid t s s -> Hybrid t s s
takeH t (Hybrid f m) = Hybrid f (aux m)
    --aux is factored out to delay evaluating the Maybe as long as possible
    where aux (Just (d, x)) = Just (min t d, if t < d then f t else x)
          aux Nothing       = Just (t, f t)

--assumes 0 <= t <= duration h
dropH :: (Num t) => t -> Hybrid t s a -> Hybrid t s a
dropH t (Hybrid f m) = Hybrid (f . (t+)) (fmap (\(d,x) -> (d-t,x)) m)

joinH :: (Num t, Ord t) => Hybrid t s a -> Hybrid t s b -> Hybrid t s b
joinH (Hybrid f Nothing) _                   = Hybrid f Nothing
joinH (Hybrid f (Just (d, _))) ~(Hybrid g m) = Hybrid h (fmap ((d+) >< id) m)
    where h t | t < d       = f t
              | otherwise   = g (t - d)


instance (Num t, Ord t) => Applicative (Hybrid t s) where
    pure  = instant
    (<*>) = ap

instance (Num t, Ord t) => Monad (Hybrid t s) where
    h@(Hybrid _ (Just (_,x)))   >>= f = joinH h (f x)
    (Hybrid e Nothing)          >>= _ = Hybrid e Nothing




newtype Query s = Query { runQuery :: Maybe Int -> NonEmpty s } deriving (Functor)

runQueryJust :: Query s -> Int -> NonEmpty s
runQueryJust q = runQuery q . Just

runQueryInf :: Query s -> s
runQueryInf q = case runQuery q Nothing of
                    (x :| []) -> x
                    _ -> error "runQueryInf: expected singleton list"

instance Applicative Query where
    pure  = Query . pure . pure
    (<*>) = ap

instance Monad Query where
    q >>= f = Query $ \n -> runQuery q n >>= (($ n) . runQuery . f)


--unline Hybrid, the function inside Hyb only needs to be
--defined in (0,d) (or (0,inf) if duration is infinite)
newtype CompHybridT t s m a = CompHybridT { unCHT :: m (Either a (Hybrid t (Query s) a)) }
type CompHybrid t s a = CompHybridT t s Identity a

unCH :: CompHybrid t s a -> Either a (Hybrid t (Query s) a)
unCH = runIdentity . unCHT

runCompHybridT :: (Functor m) => CompHybridT t s m a -> m (CompHybrid t s a)
runCompHybridT = fmap (CompHybridT . Identity) . unCHT


smapCH :: (Functor m) => (r -> s) -> CompHybridT t r m a -> CompHybridT t s m a
smapCH f = CompHybridT . fmap (id -|- smap (fmap f)) . unCHT

fsmapCH :: (Functor m) => (r -> s) -> CompHybridT t r m r -> CompHybridT t s m s
fsmapCH f = fmap f . smapCH f

instantCH :: (Applicative m) => a -> CompHybridT t s m a
instantCH = CompHybridT . pure . Left

hybridCH :: (Applicative m) => Hybrid t s a -> CompHybridT t s m a
hybridCH = CompHybridT . pure . Right . smap pure

evalCH :: CompHybrid t s a -> t -> Query s
evalCH = either err eval . unCH
    where err _ = error "evalCH: Failed to evaluate instantaneous CompHybrid"

unrollCH :: (Num t) => CompHybrid t s a -> Maybe (t, a)
unrollCH = either (Just . (0,)) unroll . unCH

durationCH :: (Num t) => CompHybrid t s a -> Maybe t
durationCH = either (const $ Just 0) duration . unCH

endpointCH :: CompHybrid t s a -> Maybe a
endpointCH = either Just endpoint . unCH

joinCH :: (Num t, CompOrd t) => Hybrid t (Query s) a -> Hybrid t (Query s) b -> Hybrid t (Query s) b
joinCH (Hybrid f Nothing) _                     = Hybrid f Nothing
joinCH (Hybrid f (Just (d, _))) ~(Hybrid g m)   = Hybrid (join . Query . h) (fmap ((d+) >< id) m)
    where h t Nothing  = if lesserInf t d then (f t) :| [] else (g (t - d)) :| []
          h t (Just n) = case mCompare (Top LT) (domCompare t d n) of
                            Just True  -> f t :| []             --The wrong branch is evaluated
                            Just False -> g (t - d) :| []       --outside its original domain.
                            Nothing    -> f t :| [g (t - d)]    --Potentially dangerous

instance (Functor m) => Functor (CompHybridT t s m) where
    fmap f = CompHybridT . fmap (f -|- fmap f) . unCHT

instance (Num t, CompOrd t, Monad m) => Applicative (CompHybridT t s m) where
    pure  = CompHybridT . return . Left
    (<*>) = ap

instance (Num t, CompOrd t, Monad m) => Monad (CompHybridT t s m) where
    mx >>= f = CompHybridT $ do
        ch <- unCHT mx
        case ch of
            Left x -> unCHT (f x)
            Right (Hybrid e Nothing) -> return $ Right $ Hybrid e Nothing
            Right h1@(Hybrid e (Just (d,x))) -> Right . aux <$> unCHT (f x)
                where aux (Left y)   = Hybrid e (Just (d,y))
                      aux (Right h2) = joinCH h1 h2

instance (Num t, CompOrd t) => MonadTrans (CompHybridT t s) where
    lift m = CompHybridT (fmap Left m)