{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Plot (PlotConfig(..), defPlotConfig, Plottable(..)) where

import Utils
import CompReal
import Lang.Hybrid
import Lang.Interpreter (RunResult, allVals)
import CompOrd

--needs packages Chart and Chart-cairo
import Graphics.Rendering.Chart.Easy hiding (points, both)
import Graphics.Rendering.Chart.Backend.Cairo
import Prelude hiding (lines)
import Control.Applicative (ZipList(..))
import Control.Monad (when)
import Data.Ratio ((%))
import Data.List (transpose, sortOn, insert)
import Data.Maybe (isJust, fromJust, catMaybes)
import GHC.Utils.Misc (sndOf3, thdOf3)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))

--TODO: chart uses Doubles to place features on the plot. We need to
--      make up some wort of workaround to avoid exhausting the precision
--      of a Double when very zoomed in


data PlotConfig = PlotConfig    { outputPath    :: String
                                , sampleNo      :: Integer
                                , precision     :: Int
                                , rangeT        :: Maybe (Rational, Rational)
                                , rangeX        :: Maybe (Rational, Rational)
                                }

defPlotConfig :: PlotConfig
defPlotConfig = PlotConfig  { outputPath    = "output.png"
                            , sampleNo      = 200
                            , precision     = 32
                            , rangeT        = Nothing
                            , rangeX        = Nothing
                            }


setLayout rT rX = do
    layout_title .= "System Evolution"
    layout_x_axis . laxis_title .= "time"
    when (isJust rT) $ layout_x_axis . laxis_generate .= scaledAxis def (fromRational >< fromRational $ fromJust rT)
    when (isJust rX) $ layout_y_axis . laxis_generate .= scaledAxis def (fromRational >< fromRational $ fromJust rX)

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

--TODO: if rangeT is specified, it shouldn't return a value greater than duration (but we also
--don't want to evaluate duration because that forces to evaluate the whole program, soooo...)
getRangeT :: (Fractional a) => Maybe (Rational, Rational) -> Maybe a -> IO (a, a)
getRangeT (Just (l,r)) _   = return (fromRational l, fromRational r)
getRangeT Nothing (Just r) = return (0, r)
getRangeT Nothing Nothing  = do
    hPutStrLn stderr "Time range not specified for infinite system"
    exitWith $ ExitFailure 1

samples :: (Fractional a) => Integer -> a -> [(Rational, a)]
samples no tf = [(s % no, tf * fromRational (s % no)) | s <- [0..no]]

segments :: [(a, RunResult (Int, b))] -> [(a, (Int, b), (Int, b))] -> [[(a, b)]]
segments evol ds = map (map snd) $ foldr groupify [] $ sortOn fst $ [((i,1),(t,x)) | (t,rr) <- evol, (i,x) <- allVals rr] ++ concat [[((i,2 :: Integer),(t,x)),((j,0),(t,y))] | (t,(i,x),(j,y)) <- ds]
    where groupify x [] = [[x]]
          groupify x@((_,2),_) xss@((((_,0),_):_):_) = [x] : xss
          groupify x@((_,2),_) ((_:xs):xss) = groupify x (xs:xss)
          groupify x (ys:yss) = (x : ys) : yss

toDouble :: (Real a) => a -> Double
toDouble = fromRational . toRational


class Plottable a b where
    plotHybrid :: PlotConfig -> [String] -> Hybrid a (RunResult (Int, [b])) -> [(a, Int, Int, [Maybe (b, b)])] -> IO ()

    default plotHybrid :: (RealFrac a, Real b) => PlotConfig -> [String] -> Hybrid a (RunResult (Int, [b])) -> [(a, Int, Int, [Maybe (b, b)])] -> IO ()
    plotHybrid config vars h discs = do
        (ti, tf) <- getRangeT (rangeT config) (duration h)
        let system = transpose [map (t,) $ getZipList $ sequenceA $ fmap (ZipList . sequenceA) $ eval h t | (_,t) <- samples (sampleNo config) tf]
        let discsByVar = map catMaybes $ transpose [map (fmap (\(x,y) -> (t,(i,x),(j,y)))) xs | (t,i,j,xs) <- takeWhile ((<= tf) . fstOf4) $ dropWhile ((< ti) . fstOf4) discs]

        toFile def (outputPath config) $ do
            setLayout (rangeT config) (rangeX config)
            sequence_ $ (<$> zip3 vars (system ++ repeat []) (discsByVar ++ repeat [])) $ \(var, evol, ds) -> do
                color <- takeColor
                lines var color $ map (map (toDouble >< toDouble)) $ segments evol ds
                hollowPoints color [(toDouble t, toDouble x) | (t,(_,x),_) <- ds]
                filledPoints color [(toDouble t, toDouble x) | (t,_,(_,x)) <- ds]

instance Plottable Double Double


approxDouble :: (CompReal r) => Int -> r -> Double
approxDouble n r = fromRational $ approx r n

boundDouble :: (CompReal r) => Int -> r -> (Double, Double)
boundDouble n r = let (l, u) = bound r n in (fromRational l, fromRational u)

instance {-# OVERLAPPABLE #-} (CompReal a, CompReal b) => Plottable a b where
    plotHybrid config vars h discs = do
        let n = precision config
        (ti, tf) <- getRangeT (rangeT config) (duration h)
        let boundRat s = (s*) >< (s*) $ boundDouble n tf
        let approxRat s = s * (approxDouble n tf)
        let system = transpose [map (fromRational s,) $ getZipList $ sequenceA $ fmap (ZipList . sequenceA) $ eval h t | (s,t) <- samples (sampleNo config) tf]
        let discsByVar = map catMaybes $ transpose [map (fmap (\(x,y) -> (t,(i,x),(j,y)))) xs | (t,i,j,xs) <- takeWhile (flip (<! tf) n . fstOf4) $ dropWhile (flip (<! ti) n . fstOf4) discs]

        toFile def (outputPath config) $ do
            setLayout (rangeT config) (rangeX config)
            sequence_ $ (<$> zip3 vars (system ++ repeat []) (discsByVar ++ repeat [])) $ \(var, evol, ds) -> do
                color <- takeColor
                let segs = segments [(Left t, rr) | (t,rr) <- evol] [(Right t, l, r) | (t,l,r) <- ds]
                joinRects var color [[(either boundRat (boundDouble n) t, boundDouble n v) | (t,v) <- vs] | vs <- segs]
                lines var color [[(either approxRat (approxDouble n) t, approxDouble n v) | (t,v) <- vs] | vs <- segs]
                hollowPoints color [(approxDouble n t, approxDouble n x) | (t,(_,x),_) <- ds]
                filledPoints color [(approxDouble n t, approxDouble n x) | (t,_,(_,x)) <- ds]