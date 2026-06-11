--
-- This file is used to generate visualizations for some hybrid programs using
-- library diagrams. There is no built-in way to specify a visualization
-- automatically with Jaguar, all diagrams here are hard-coded for specific programs
--

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE DefaultSignatures         #-}

module Diagrams (
    Composition,
    MultiComposition,
    unMulti,
    circleBilliard,
    sinaiBilliard,
    sinaiBilliardMulti,
    pendulumBalancing,
    pendulumBalancingMulti,
    toSVG,
    toSVGCompare,
    animateSVG,
    animateSVGCompare,
    toGIF,
    toGIFCompare) where

import Utils
import CompReal
import Powers
import Boundable
import Lang.Hybrid
import Lang.Interpreter

import Diagrams.Prelude hiding ((.-.))
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Cairo.CmdLine
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)
import Data.List (findIndex, intersperse)
import qualified Data.List.NonEmpty as NE

class (SimNum r, Show r) => DiagramNum r where
    toDouble :: r -> Double

    default toDouble :: (Real r) => r -> Double
    toDouble = fromRational . toRational

instance DiagramNum Double

instance {-# OVERLAPPABLE #-} (CompReal r, Powers r, Boundable r, Show r) => DiagramNum r where
    toDouble = fromRational . (`approx` 10)


-------BACKGROUNDS-------

sinaiBackground :: Diagram B
sinaiBackground = bgFrame 0.25 white $ circle 1 # lc green # fc lightgreen `atop` square 4 # lc green

circleBackground :: Diagram B
circleBackground = bgFrame 0.25 white $ circle 1 # lc green


-------SIMULATORS-------

billiard :: Colour Double -> [String] -> [Double] -> Diagram B
billiard color vars state = translate (r2 (x, y)) (ball `atop` _arrow)
    where ball = circle 0.05 # fc color
          _arrow = scale 0.3 (arrowV' arrowStyle (V2 vx vy))
          arrowStyle = with & headLength .~ local 0.3
          getVar var = state !! (fromJust $ findIndex (==var) vars)
          x = getVar "x"
          y = getVar "y"
          vx = getVar "vx"
          vy = getVar "vy"

invertedPendulum :: Colour Double -> [String] -> [Double] -> Diagram B
invertedPendulum color vars state = rotate (a @@ rad) $ (translate (r2 (0, 1)) $ _arrow `atop` ball) `atop` fromOffsets [r2 (0,0), r2 (0,1)]
    where ball = circle 0.05 # fc color
          _arrow = if va == 0 then mempty else scale 0.3 (arrowV' arrowStyle (V2 (-va) 0))
          arrowStyle = with & headLength .~ local (min 0.15 (0.5 * abs va))
          getVar var = state !! (fromJust $ findIndex (==var) vars)
          a = getVar "theta"
          va = getVar "w"


-------COMPOSITIONS-------

colors :: [Colour Double]
colors = cycle [red, blue, green, magenta, yellow, aqua]

type Composition = [String] -> [Double] -> Diagram B
type MultiComposition = [String] -> [[Double]] -> Diagram B

unMulti :: MultiComposition -> Composition
unMulti f vars state = f vars [state]


circleBilliard :: Composition
circleBilliard vars state = billiard red vars state `atop` circleBackground

sinaiBilliard :: Composition
sinaiBilliard vars state = billiard red vars state `atop` sinaiBackground

sinaiBilliardMulti :: MultiComposition
sinaiBilliardMulti vars states = mconcat (zipWith (\c s -> billiard c vars s) colors states) `atop` sinaiBackground

pendulumBalancing :: Composition
pendulumBalancing vars state = bgFrame 0.25 white $ invertedPendulum red vars state `atop` hrule 2

pendulumBalancingMulti :: MultiComposition
pendulumBalancingMulti vars states = bgFrame 0.25 white $ mconcat (zipWith (\c s -> invertedPendulum c vars s) colors states) `atop` hrule 2



-------HELPER FUNCS-------

difs :: (Num a) => [a] -> [a]
difs (x:xs) = zipWith (-) xs (x:xs)
difs [] = error "expected non-empty list"

timecode :: (Show t) => t -> Diagram B
timecode t = moveTo (p2 (-1,1)) $ scale 0.05 $ topLeftText ("t=" ++ show t)

runProg :: (DiagramNum r) => [String] -> RunnableProgram r -> r -> [r]
runProg vars prog = snd . (!! 0) . NE.toList . (`runQueryJust` 10) . evalCH hyb
    where (hyb, _) = run prog (length vars) (Just 10) Nothing


-------EXPORT FUNCS-------

toSVG :: (DiagramNum r) => Composition -> [String] -> RunnableProgram r -> r -> IO ()
toSVG f vars prog t = mainWith $ f vars (map toDouble $ runProg vars prog t)

toSVGCompare :: (DiagramNum r, DiagramNum s, Real t) => MultiComposition -> [String] -> RunnableProgram r -> RunnableProgram s -> t -> IO ()
toSVGCompare f vars prog1 prog2 t = mainWith $ f vars [getState prog1, getState prog2]
    where getState :: (forall r. DiagramNum r => RunnableProgram r -> [Double])
          getState prog = map toDouble $ runProg vars prog $ fromRational $ toRational t

animateSVG :: (DiagramNum r) => Composition -> [String] -> RunnableProgram r -> IO ()
animateSVG f vars prog = sequence_ $ intersperse (threadDelay delay) $ map draw ts
    where draw t = toSVG ((timecode t `atop`) .-. f) vars prog t
          ts = map fromRational [0,0.5..]
          delay = 500000 --in microseconds

animateSVGCompare :: (DiagramNum r, DiagramNum s) => MultiComposition -> [String] -> RunnableProgram r -> RunnableProgram s -> IO ()
animateSVGCompare f vars prog1 prog2 = sequence_ $ intersperse (threadDelay delay) $ map draw ts
    where draw t = toSVGCompare ((timecode t `atop`) .-. f) vars prog1 prog2 t
          ts = map fromRational [0,0.5..] :: [Double]
          delay = 500000 --in microseconds

toGIF :: (DiagramNum r) => Composition -> [String] -> RunnableProgram r -> [r] -> IO ()
toGIF f vars prog ts = mainWith [(drawing t, _round (toDouble (100 * d))) | (t,d) <- zip ts (difs ts ++ [1])]
    where drawing t = timecode t `atop` f vars (getState t)
          getState = map toDouble . runProg vars prog
          _round = round :: Double -> Int

toGIFCompare :: forall r s t. (DiagramNum r, DiagramNum s, Real t, Show t) => MultiComposition -> [String] -> RunnableProgram r -> RunnableProgram s -> [t] -> IO ()
toGIFCompare f vars prog1 prog2 ts = mainWith [(drawing t, _round (toRational (100 * d))) | (t,d) <- zip ts (difs ts ++ [1])]
    where drawing t = timecode t `atop` f vars [getState1 t, getState2 t]
          getState :: (forall q. DiagramNum q => RunnableProgram q -> t -> [Double])
          getState prog = map toDouble . runProg vars prog . fromRational . toRational
          getState1 = getState prog1
          getState2 = getState prog2
          _round = round :: Rational -> Int