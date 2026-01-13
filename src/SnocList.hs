{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{-
This module represents lists as a body and a last element

This is useful when the last element of a (finite) list should be treated in some
special way (e.g. functions findOrLast and repeatLast). It also has performance
benefits when the last element is repeatedly accessed

Because of Haskell's laziness, infinite lists are represented as SnocList (...) _|_
Therefore, always keep in mind that evaluating the last element of a SnocList
is equivalent to iterating the full list that created it, and if that list is
infinite that operation will never halt
-}

module SnocList (
    SnocList(..),
    singleton,
    fromList,
    toList,
    foldr,
    unfoldr,
    head,
    tailOrLast,
    last,
    drop,
    dropOrLast,
    takeUntil,
    mapOrLast,
    zip,
    zipOrLast,
    zip3,
    zipWith,
    zipOrLastWith,
    zipWith3,
    iterate,
    indexOrLast,
    indexOrLastMemo,
    findOrLast,
    repeatLast,
    getIndexesOrLast) where

import Utils
import Shortcut

import Prelude hiding (foldr, head, last, take, drop, zip, zipWith, zip3, zipWith3, iterate)
import qualified Prelude as P
import Data.List (unsnoc, find)
import Data.Maybe (fromMaybe)


data SnocList a = SnocList [a] a deriving (P.Functor, P.Foldable, P.Traversable)

instance (Show a) => Show (SnocList a) where
    showsPrec p xs = showParen (p > 10) $ showString "fromList " . shows (toList xs)

lFunc :: ([a] -> [a]) -> SnocList a -> SnocList a
lFunc f ~(SnocList xs y) = SnocList (f xs) y

singleton :: a -> SnocList a
singleton y = SnocList [] y

fromList :: [a] -> SnocList a
fromList = maybe (error "fromList: expected non-empty list") (uncurry SnocList) . unsnoc

toList :: SnocList a -> [a]
toList (SnocList xs y) = xs ++ [y]
--toList = foldr (:) singleton

foldr :: (a -> b -> b) -> (a -> b) -> SnocList a -> b
foldr f g (SnocList xs y) = P.foldr f (g y) xs

unfoldr :: (b -> Either a (a, b)) -> b -> SnocList a
unfoldr f = aux
    where aux x = either singleton (\(a,b) -> lFunc (a:) (aux b)) (f x)

head :: SnocList a -> a
head (SnocList (x:_) _) = x
head (SnocList [] y)    = y

tailOrLast :: SnocList a -> SnocList a
tailOrLast (SnocList (_:xs) y) = SnocList xs y
tailOrLast sl = sl

last :: SnocList a -> a
last (SnocList _ y) = y

drop :: Int -> SnocList a -> [a]
drop i = P.drop i . toList

dropOrLast :: Int -> SnocList a -> SnocList a
dropOrLast i = lFunc (P.drop i)

--creates a SnocList whose last element is the first on the list to satisfy the given property
takeUntil :: (a -> Bool) -> [a] -> SnocList a
takeUntil f = P.foldr aux (error "takeUntil: no element in list satisfies property")
    where aux x rec | f x       = SnocList [] x
                    | otherwise = lFunc (x:) rec

mapOrLast :: (a -> b) -> (a -> b) -> SnocList a -> SnocList b
mapOrLast f g (SnocList xs y) = SnocList (map f xs) (g y)

zip :: SnocList a -> SnocList b -> SnocList (a,b)
zip = zipWith (,)

zipWith :: (a -> b -> c) -> SnocList a -> SnocList b -> SnocList c
zipWith f = aux
    where aux (SnocList (w:ws) x) (SnocList (y:ys) z) = lFunc (f w y :) $ aux (SnocList ws x) (SnocList ys z)
          aux sl1 sl2 = singleton $ f (head sl1) (head sl2)

zipOrLast :: SnocList a -> SnocList b -> SnocList (a,b)
zipOrLast = zipOrLastWith (,)

zipOrLastWith :: (a -> b -> c) -> SnocList a -> SnocList b -> SnocList c
zipOrLastWith f = aux
    where aux (SnocList (w:ws) x) (SnocList (y:ys) z) = lFunc (f w y :) $ aux (SnocList ws x) (SnocList ys z)
          aux (SnocList [] x) (SnocList [] z) = singleton (f x z)
          aux sl1 sl2 = lFunc (f (head sl1) (head sl2) :) $ aux (tailOrLast sl1) (tailOrLast sl2)

zip3 :: SnocList a -> SnocList b -> SnocList c -> SnocList (a,b,c)
zip3 = zipWith3 (,,)

zipWith3 :: (a -> b -> c -> d) -> SnocList a -> SnocList b -> SnocList c -> SnocList d
zipWith3 f = aux
    where aux (SnocList (u:us) v) (SnocList (w:ws) x) (SnocList (y:ys) z) = lFunc (f u w y :) $ aux (SnocList us v) (SnocList ws x) (SnocList ys z)
          aux sl1 sl2 sl3 = singleton $ f (head sl1) (head sl2) (head sl3)

iterate :: (a -> a) -> a -> SnocList a
iterate f x = SnocList (P.iterate f x) undefined

indexOrLast :: SnocList a -> Int -> a
indexOrLast sl i = head $ dropOrLast i sl

--when using, watch out for polymorphism, since free type variables prevent memoization
--doesn't work:     f = indexOrLastMemo (fromList [0..100000000]);                  f 200000000
--but this works:   f = indexOrLastMemo (fromList [0..100000000] :: SnocList Int);  f 200000000
indexOrLastMemo :: SnocList a -> Int -> a
indexOrLastMemo (SnocList xs y) = readShortFunc short
    where short = makeShort beforeOpen afterOpen
          beforeOpen open i = either id (\l -> open l `seq` y) $ indexOrLength xs i
          afterOpen l i = if i < l then xs !! i else y


--TODO: memoize more indexes and not just last?
{-
data LeafTree a = Leaf a | LNode (LeafTree a) | Node (LeafTree a) (LeafTree a)
indexOrLastMemo xs = search
  where trees = map (uncurry buildTree) $ splitPow xs
        splitPow xs = unfoldr aux (1,xs)
            where aux (n,[]) = Nothing
                  aux (n,ys) = let (l,r) = splitAt n ys in Just ((n,l),(2*n,r))
        buildTree _ [] = Empty
        buildTree n xs = Node (head r) (buildTree m l) (buildTree m (tail r))
          where m = n `div` 2
                (l,r) = splitAt m xs
        search n = indexTree (n + 1 - 2^i) (2^i) $ indexOrLast trees i
            where i = lg2 $ toInteger $ n + 1
        lastTree (Leaf x) = x
        lastTree (LNode t) = lastTree t
        lastTree (Node _ t) = lastTree t
        indexTree 0 _ (Leaf x) = x
        indexTree n m (Node l r) | 2 * n < m = indexTree (2 * n) m l
                                 | otherwise = indexTree (2 * n - m) m r
-}


findOrLast :: (a -> Bool) -> SnocList a -> a
findOrLast f (SnocList xs y) = fromMaybe y (find f xs)

repeatLast :: SnocList a -> [a]
repeatLast (SnocList xs y)  = xs ++ P.repeat y
--repeatLast = foldr (:) P.repeat

--indexes must be increasing. if there are multiple indexes greater
--than the size of the list, the last element is only appended once
getIndexesOrLast :: [Int] -> SnocList a -> SnocList a
getIndexesOrLast []       = singleton . last
getIndexesOrLast (i:is)   = aux (i : difs)
    where difs = P.zipWith (-) is (i:is)
          aux [] (SnocList _ y) = singleton y
          aux _ (SnocList [] y) = singleton y
          aux (d:ds) sl = case dropOrLast d sl of
                                (SnocList [] y)         -> singleton y
                                sl2@(SnocList (x:_) _)  -> lFunc (x:) (aux ds sl2)