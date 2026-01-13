{-# LANGUAGE DeriveFunctor #-}

module Shortcut (Shortcut, makeShort, readShort, readShortFunc) where

import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import System.IO.Unsafe (unsafePerformIO)

{-
Shortcut represents a computation that can be calculated faster if/when some
condition has been met. It is trivial to create values that contain ongoing
computations (that is the whole concept behind thunks, and SnocList is a more
specific example), but Shortcut allows the direct manipulation of the ongoing
computation in a way otherwise impossible, hence the use of unsafe functions

For an example of Shortcut in action, check SnocList.indexOrLastMemo
Currently it's the only place that uses Shortcut
-}


data Shortcut p a = Shortcut (MVar (Maybe p)) a (p -> a) deriving (Functor)


--The shortcutted computation f receives a function to open the shortcut
--The result of openShortcut must be seq'ed or pattern matched to force the evaluation
--Obviously, f and g must always produce equal values to maintain consistency
makeShort :: ((p -> ()) -> a) -> (p -> a) -> Shortcut p a
makeShort f g = unsafePerformIO $ do
    var <- newMVar Nothing
    let openShortcut = \p -> unsafePerformIO $ modifyMVar_ var (const $ return $ Just p) >> return ()
    return $ Shortcut var (f openShortcut) g

--If the shortcut has been opened, use it
--If not, perform the computation
readShort :: Shortcut p a -> a
readShort (Shortcut var x g) = unsafePerformIO $ do
    mP <- readMVar var
    case mP of
        Nothing -> return x
        Just p  -> return $ g p

--Same as readShort, but checks for the shortcut for every call to the function
readShortFunc :: Shortcut p (a -> b) -> a -> b
readShortFunc (Shortcut var f g) x = unsafePerformIO $ do
    mP <- readMVar var
    case mP of
        Nothing -> return $ f x
        Just p  -> return $ g p x