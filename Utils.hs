module Utils where

(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(><) f g (x, y) = (f x, g y)

fstOf4 :: (a, b, c, d) -> a
fstOf4 (x,_,_,_) = x

--inserts an element into an ordered list. if the element already exists in the list, nothing changes
setInsert :: Ord a => a -> [a] -> [a]
setInsert x [] = [x]
setInsert x (h : t) | x < h = x : h : t
                    | x > h = h : setInsert x t
                    | otherwise = h : t

--TODO: make it strict?
replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex i x xs = take i xs ++ x : drop (i+1) xs

--list must be ordered by index and mustn't contain repeated indexes
maybeIndexes :: [(Int,a)] -> [Maybe a]
maybeIndexes xs = rec 0 xs
    where rec n [] = repeat Nothing
          rec n ((i, x) : t) | n == i    = Just x  : rec (n + 1) t
                             | otherwise = Nothing : rec (n + 1) ((i, x) : t)