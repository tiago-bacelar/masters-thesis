{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Group (module Group) where

import Utils

import Data.Maybe (fromJust, isNothing)
import Data.List (transpose)
import qualified Data.List.NonEmpty as NE


--This module defines Group and OptGroup and
--provides utilities to produce and consume them
newtype Group p a = Group { unG :: [(p, a)] } deriving (Show, Functor, Foldable, Traversable)
data OptGroup p a = Grouped (Group p a) | Single a deriving (Show, Functor, Foldable, Traversable)

pmap :: (p -> q) -> Group p a -> Group q a
pmap f = Group . map (f >< id) . unG

pmapOpt :: (p -> q) -> OptGroup p a -> OptGroup q a
pmapOpt f (Grouped g) = Grouped (pmap f g)
pmapOpt _ (Single x)  = Single x

fromGrouped :: OptGroup p a -> Group p a
fromGrouped (Grouped g) = g
fromGrouped _ = error "fromGrouped: expected Grouped"

fromSingle :: OptGroup p a -> a
fromSingle (Single x) = x
fromSingle _ = error "fromSingle: expected Single"

unOptG :: OptGroup p a -> [(p, a)]
unOptG (Grouped g) = unG g
unOptG (Single _)  = []

maybeGrouped :: p -> OptGroup p a -> [(p, a)]
maybeGrouped _ (Grouped g) = unG g
maybeGrouped p (Single x)  = [(p, x)]


--PRODUCTION

groupSplit :: (Eq p) => [(p,a)] -> Group p [a]
groupSplit = Group . map (split (fst . NE.head) (map snd . NE.toList)) . NE.groupWith fst

optGroupSplit :: (Eq p) => [(Maybe p, a)] -> OptGroup p [a]
optGroupSplit xys | all isNothing (map fst xys) = Single $ map snd xys
                  | otherwise = Grouped $ groupSplit $ map (fromJust >< id) xys

--CONSUMPTION

optGroups :: Group a (OptGroup b c) -> Group a (Group b c)
optGroups (Group xs) = Group [(p, g) | (p, Grouped g) <- xs]

--assumes all groups have the same keys
swapGroup :: Group a (Group b c) -> Group b (Group a c)
swapGroup (Group []) = error "swapGroup: expected non-empty group"
swapGroup (Group xs@((_,Group ys):_)) = Group $ zip bs $ map (Group . zip as) $ transpose $ map (map snd . unG . snd) xs
    where as = map fst xs
          bs = map fst ys

--assumes either all opt groups exist (and have the same keys) or none exist
swapOptGroup :: Group a (OptGroup b c) -> OptGroup b (Group a c)
swapOptGroup (Group []) = error "swapOptGroup: expected non-empty group"
swapOptGroup g@(Group ((_,(Grouped _)):_)) = Grouped $ swapGroup $ fmap fromGrouped g
swapOptGroup g = Single $ fmap fromSingle g

-- fixAll :: (a -> b -> c) -> Group a b -> [c]
-- fixAll f = (>>= uncurry f) . unG

-- --select :: 

-- single :: a -> ([(a, b)] -> c) -> b -> c
-- single a f b = f [(a, b)]

-- multi :: (Applicative f) => (f [(a, b)] -> c) -> Group a (f b) -> c
-- multi f = f . fmap unG . sequenceA

-- ifOpt :: (Monoid c) => ([(a, b)] -> c) -> OptGroup a b -> c
-- ifOpt f (Grouped g) = f (unG g)
-- ifOpt _ (Single _)  = mempty

-- ifOptElse :: ([(a, b)] -> c) -> (b -> c) -> OptGroup a b -> c
-- ifOptElse f1 _ (Grouped g) = f1 (unG g)
-- ifOptElse _ f2 (Single x)  = f2 x

