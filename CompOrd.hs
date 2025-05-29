{-# LANGUAGE DefaultSignatures #-}

module CompOrd where

import Data.Maybe

class CompOrd a where
    --if mCompare x y n = Just c, then mCompare x y (n+1) = Just c as well
    mCompare :: a -> a -> Int -> Maybe Ordering
    infCompare :: a -> a -> Ordering
    (<!) :: a -> a -> Int -> Bool
    (>!) :: a -> a -> Int -> Bool

    default mCompare :: (Ord a) => a -> a -> Int -> Maybe Ordering
    mCompare x y _ = Just (compare x y)
    infCompare x y = head $ catMaybes $ map (mCompare x y) [0..]
    x <! y = maybe False (LT ==) . mCompare x y
    x >! y = maybe False (GT ==) . mCompare x y

    {-# MINIMAL mCompare #-}

instance CompOrd Double
instance CompOrd Integer