module Plot where

import Lang.Hybrid
import CompReal

--using https://hackage.haskell.org/package/Chart
--needs packages Chart and Chart-cairo
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Monad
import Data.Ratio
import Data.Maybe

fillBetween caption vs = liftEC $ do
  plot_fillbetween_title .= caption
  color <- takeColor
  plot_fillbetween_style .= solidFillStyle color
  plot_fillbetween_values .= vs


toDouble :: (Real a) => a -> Double
toDouble = fromRational . toRational

plotHybrid :: (RealFrac a, Real b) => [String] -> Hybrid a [b] -> IO ()
plotHybrid vars h = toFile def "output.png" $ do
    let samples = 200
    let tf = fromJust (duration h) --only supports finite programs
    let ts = map ((tf*) . fromRational . (% samples)) [0..samples]
    let evol = [(toDouble t, map toDouble $ eval h t) | t <- ts]
    layout_title .= "System Evolution"
    sequence_ [plot $ line var $ [map (fmap (!!i)) evol] | (i, var) <- zip [0..] vars]


approxDouble :: (CompReal r) => Int -> r -> Double
approxDouble n r = fromRational $ approx r n

boundDouble :: (CompReal r) => Int -> r -> (Double, Double)
boundDouble n r = let (l, u) = bound r n in (fromRational l, fromRational u)

plotHybridCR :: (CompReal a, CompReal b) => [String] -> Hybrid a [b] -> Int -> IO ()
plotHybridCR vars h n = toFile def "output.png" $ do
    let samples = 300
    --TODO: precision of the time????
    let tf = fromJust (duration h) --only supports finite programs
    let ts = map ((0.0001+) . (tf*) . fromRational . (% samples)) $ reverse [0..samples] --TODO: order of evals and epsilon
    let evol = [(approxDouble n t, map (boundDouble n) $ eval h t) | t <- ts]
    layout_title .= "System Evolution"
    sequence_ [plot $ fillBetween var $ map (fmap (!!i)) evol | (i, var) <- zip [0..] vars]
