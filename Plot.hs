module Plot (plotHybrid, plotHybridCR) where

import Utils
import CompReal
import Lang.Hybrid
import Lang.Interpreter
import CompOrd

--using https://hackage.haskell.org/package/Chart
--needs packages Chart and Chart-cairo
import Graphics.Rendering.Chart.Easy hiding (points, both)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Drawing
import Prelude hiding (lines)
import Control.Applicative (ZipList(..))
import Data.Ratio ((%))
import Data.List (transpose, sortOn, insert)
import Data.Maybe (isJust, fromJust)
import GHC.Data.Maybe (orElse)
import GHC.Utils.Misc (fstOf3, sndOf3, thdOf3)
import GHC.Exts (groupWith)


outputPath :: String
outputPath = "output.png"

sampleNo :: Integer
sampleNo = 200

samples :: (Fractional a) => a -> [(Rational, a)]
samples tf = [(s % sampleNo, tf * fromRational (s % sampleNo)) | s <- [0..sampleNo]]


setLayout = do
    layout_title .= "System Evolution"
    layout_x_axis . laxis_title .= "time"

lineStyle n colour = line_width .~ n
                   $ line_color .~ colour
                   $ def

--lines :: String -> AlphaColour Double -> [[(Double,Double)]] -> ...
lines caption color vss = plot $ liftEC $ do
    plot_lines_title .= caption
    plot_lines_style .= lineStyle 1 color
    plot_lines_values .= vss

--fillBetween :: String -> AlphaColour Double -> [[(Double, (Double, Double))]] -> ...
fillBetween caption color vss = capt >> sequence_ (map fill vss)
    where tcolor = dissolve 0.4 color
          capt = plot $ liftEC $ do
                    plot_fillbetween_style .= solidFillStyle tcolor
                    plot_fillbetween_title .= caption
          fill vs = plot $ liftEC $ do
                    plot_fillbetween_style .= solidFillStyle tcolor
                    plot_fillbetween_values .= vs

--rectangles :: String -> AlphaColour Double -> [(Double, Double, Double, Double)] -> ...
rectangles caption color rects = capt >> sequence_ (map fill rects)
    where tcolor = dissolve 0.4 color
          capt = plot $ liftEC $ do
                    plot_fillbetween_style .= solidFillStyle tcolor
                    plot_fillbetween_title .= caption
          fill ((t1,t2),(x1,x2)) = plot $ liftEC $ do
                    plot_fillbetween_style .= solidFillStyle tcolor
                    plot_fillbetween_values .= [(t1,(x1,x2)),(t2,(x1,x2))]

--joinRects :: String -> AlphaColour Double -> [[((Double, Double), (Double, Double))]] -> ...
joinRects caption color rectss = capt >> sequence_ (map fill rectss)
    where tcolor = dissolve 0.4 color
          capt = plot $ liftEC $ do
                    plot_fillbetween_style .= solidFillStyle tcolor
                    plot_fillbetween_title .= caption
          fill rects = plot $ liftEC $ do
                    plot_fillbetween_style .= solidFillStyle tcolor
                    plot_fillbetween_values .= fillAreas (sortOn (fst . fst) rects) []
          fillAreas [] [] = []
          fillAreas (((t1,t2),(x1,x2)):rects) [] = (t1,(x1,x2)) : fillAreas rects [(t2,x1,x2)]
          fillAreas [] ss@((t,_,_):ss') = (t, barX ss) : fillAreas [] ss'
          fillAreas rs@(((t1,t2),(x1,x2)):rs') ss@((t,_,_):ss') 
            | t1 < t = let ss'' = insert (t2,x1,x2) ss in (t1, barX ss'') : fillAreas rs' ss''
            | t < t1 = (t, barX ss) : fillAreas rs ss'
            | otherwise = let ss'' = insert (t2,x1,x2) ss in (t, barX ss'') : fillAreas rs' (tail ss'')
          barX ss = (minimum $ map sndOf3 ss, maximum $ map thdOf3 ss)

--hollowPoints :: AlphaColour Double -> [(Double, Double)] -> ...
hollowPoints color vs = plot $ liftEC $ do
    plot_points_style .= hollowCircles 3 1 color
    plot_points_values .= vs

--filledPoints :: AlphaColour Double -> [(Double, Double)] -> ...
filledPoints color vs = plot $ liftEC $ do
    plot_points_style .= filledCircles 3.5 color
    plot_points_values .= vs


segments :: (Show a, Show b) => [(a, RunResult (Int, Maybe b))] -> [[(a, b)]]
segments evol = map (map snd) $ groupWith fst $ sortOn fst [(i, (t, fromJust m)) | (t,rr) <- evol, (i,m) <- allVals rr, isJust m]


toDouble :: (Real a) => a -> Double
toDouble = fromRational . toRational

plotHybrid :: (RealFrac a, Real b, Show a, Show b) => [String] -> Hybrid a (RunResult (Int, [Maybe b])) -> [(a, (Int, [Maybe b]), (Int, [Maybe b]))] -> IO ()
plotHybrid vars h discs = do
    let tf = duration h `orElse` error "Tried to plot infinite system"
    let system = transpose [map (t,) $ getZipList $ sequenceA $ fmap (ZipList . sequenceA) $ eval h t | (_,t) <- samples tf]
    let discsByVar = transpose [zip3 (repeat t) (sequenceA xs) (sequenceA ys) | (t,xs,ys) <- takeWhile ((<= tf) . fstOf3) $ dropWhile ((< 0) . fstOf3) discs]

    toFile def outputPath $ do
        setLayout
        sequence_ $ (<$> zip3 vars (system ++ repeat []) (discsByVar ++ repeat [])) $ \(var, evol, ds) -> do
            color <- takeColor
            lines var color $ map (map (toDouble >< toDouble)) $ segments evol
            hollowPoints color $ [(toDouble t, toDouble $ fromJust x) | (t,(_,x),_) <- ds, isJust x]
            filledPoints color $ [(toDouble t, toDouble $ fromJust x) | (t,_,(_,x)) <- ds, isJust x]


approxDouble :: (CompReal r) => Int -> r -> Double
approxDouble n r = fromRational $ approx r n

boundDouble :: (CompReal r) => Int -> r -> (Double, Double)
boundDouble n r = let (l, u) = bound r n in (fromRational l, fromRational u)

plotHybridCR :: (CompReal a, CompReal b, Show a, Show b) => [String] -> Hybrid a (RunResult (Int, [Maybe b])) -> [(a, (Int, [Maybe b]), (Int, [Maybe b]))] -> Int -> IO ()
plotHybridCR vars h discs n = do
    let tf = duration h `orElse` error "Tried to plot infinite system"
    let (tfl, tfu) = boundDouble n tf
    let tfm = approxDouble n tf
    let boundRat s = (s * tfl, s * tfu)
    let approxRat s = s * tfm
    let system = transpose [map (fromRational s,) $ getZipList $ sequenceA $ fmap (ZipList . sequenceA) $ eval h t | (s,t) <- samples tf]
    let discsByVar = transpose [zip3 (repeat t) (sequenceA xs) (sequenceA ys) | (t,xs,ys) <- takeWhile (flip (<! tf) n . fstOf3) $ dropWhile (not . flip (>! 0) n . fstOf3) discs]

    toFile def outputPath $ do
        setLayout
        sequence_ $ (<$> zip3 vars (system ++ repeat []) (discsByVar ++ repeat [])) $ \(var, evol, ds) -> do
            color <- takeColor
            let segs = segments $ (map (Left><id) evol) ++ [(Right t, Val v) | (t,l,r) <- ds, v <- [l,r]]

            joinRects var color [[(either boundRat (boundDouble n) t, boundDouble n v) | (t,v) <- vs] | vs <- segs]
            lines var color [[(either approxRat (approxDouble n) t, approxDouble n v) | (t,v) <- vs] | vs <- segs]
            hollowPoints color $ [(approxDouble n t, approxDouble n $ fromJust x) | (t,(_,x),_) <- ds, isJust x]
            filledPoints color $ [(approxDouble n t, approxDouble n $ fromJust x) | (t,_,(_,x)) <- ds, isJust x]