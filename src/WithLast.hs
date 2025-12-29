{-# LANGUAGE DeriveFunctor #-}

{-
This module represents lists as a body and a last element

This is useful when the last element of a (finite) list should be treated in some
special way (e.g. functions findOrLast and repeatLast)

Because of Haskell's laziness, infinite lists are represented as WithLast (...) _|_
Therefore, always keep in mind that evaluating the last element of a WithLast
is equivalent to iterating the full list that created it, and if that list is
infinite that operation will never halt
-}

module WithLast (
    WithLast,
    singleton,
    fromList,
    toList,
    foldr,
    unfoldr,
    head,
    last,
    drop,
    dropOrLast,
    zip,
    zip3,
    zipWith,
    zipWith3,
    iterate,
    indexOrLast,
    indexOrLastMemo,
    findOrLast,
    repeatLast,
    getIndexesOrLast) where

import Prelude hiding (foldr, head, last, take, drop, zip, zipWith, zip3, zipWith3, iterate)
import qualified Prelude as P
import Data.List (find)
import Data.Maybe (fromMaybe)


data WithLast a = WithLast [a] a deriving (P.Functor)

lFunc :: ([a] -> [a]) -> WithLast a -> WithLast a
lFunc f (WithLast xs y) = WithLast (f xs) y

singleton :: a -> WithLast a
singleton y = WithLast [] y

fromList :: [a] -> WithLast a
fromList [] = error "fromList: expected non-empty list"
fromList [x] = WithLast [] x
fromList (x:xs) = lFunc (x:) (fromList xs)

toList :: WithLast a -> [a]
toList (WithLast xs y) = xs ++ [y]
--toList = foldr (:) singleton

foldr :: (a -> b -> b) -> (a -> b) -> WithLast a -> b
foldr f g (WithLast xs y) = P.foldr f (g y) xs

unfoldr :: (b -> Either a (a, b)) -> b -> WithLast a
unfoldr f = aux
    where aux x = either singleton (\(a,b) -> lFunc (a:) (aux b)) (f x)

head :: WithLast a -> a
head (WithLast (x:_) _) = x
head (WithLast [] y)    = y

last :: WithLast a -> a
last (WithLast _ y) = y

drop :: Int -> WithLast a -> [a]
drop i = P.drop i . toList

dropOrLast :: Int -> WithLast a -> WithLast a
dropOrLast i = lFunc (P.drop i)

zip :: WithLast a -> WithLast b -> WithLast (a,b)
zip = zipWith (,)

zipWith :: (a -> b -> c) -> WithLast a -> WithLast b -> WithLast c
zipWith f = aux
    where aux (WithLast (w:ws) x) (WithLast (y:ys) z) = lFunc (f w y :) $ aux (WithLast ws x) (WithLast ys z)
          aux wl1 wl2 = singleton $ f (head wl1) (head wl2)

zip3 :: WithLast a -> WithLast b -> WithLast c -> WithLast (a,b,c)
zip3 = zipWith3 (,,)

zipWith3 :: (a -> b -> c -> d) -> WithLast a -> WithLast b -> WithLast c -> WithLast d
zipWith3 f = aux
    where aux (WithLast (u:us) v) (WithLast (w:ws) x) (WithLast (y:ys) z) = lFunc (f u w y :) $ aux (WithLast us v) (WithLast ws x) (WithLast ys z)
          aux wl1 wl2 wl3 = singleton $ f (head wl1) (head wl2) (head wl3)

iterate :: (a -> a) -> a -> WithLast a
iterate f x = WithLast (P.iterate f x) undefined

indexOrLast :: WithLast a -> Int -> a
indexOrLast wl i = head $ dropOrLast i wl

--when using, watch out for polymorphism
indexOrLastMemo :: WithLast a -> Int -> a
indexOrLastMemo = indexOrLast --TODO
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


findOrLast :: (a -> Bool) -> WithLast a -> a
findOrLast f (WithLast xs y) = fromMaybe y (find f xs)

repeatLast :: WithLast a -> [a]
repeatLast (WithLast xs y)  = xs ++ P.repeat y

--indexes must be increasing. if there are multiple indexes greater
--than the size of the list, the last element is only appended once
getIndexesOrLast :: [Int] -> WithLast a -> WithLast a
getIndexesOrLast []       = \(WithLast _ y) -> WithLast [] y
getIndexesOrLast (i:is)   = aux (i : difs)
    where difs = map (uncurry (-)) $ P.zip is (i:is)
          aux [] (WithLast _ y) = WithLast [] y
          aux _ (WithLast [] y) = WithLast [] y
          aux (d:ds) wl = case dropOrLast d wl of
                                (WithLast [] y)     -> WithLast [] y
                                (WithLast (x:_) _)  -> lFunc (x:) (aux ds wl)