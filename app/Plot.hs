{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Plot (PlotConfig(..), defPlotConfig, Plottable(..)) where

import Utils
import CompOrd
import CompReal
import Lang.Hybrid
import Lang.Interpreter (Step)

--needs packages Chart and Chart-cairo
import Graphics.Rendering.Chart.Easy hiding (points, both)
import Graphics.Rendering.Chart.Backend.Cairo
import Prelude hiding (lines)
import Control.Applicative (ZipList(..))
import Control.Monad (when)
import Data.Ratio ((%))
import Data.List (transpose, sortOn)
import Data.List.NonEmpty (NonEmpty(..), insert, toList)
import Data.Maybe (isJust, fromJust, catMaybes)
import GHC.Utils.Misc (sndOf3, thdOf3)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))

--TODO: chart uses Doubles to place features on the plot. We need to
--      make up some sort of workaround to avoid exhausting the precision
--      of a Double when very zoomed in


data PlotConfig = PlotConfig    { outputPath    :: String
                                , sampleNo      :: Integer
                                , queryAccuracy :: Maybe Int
                                , realAccuracy  :: Int
                                , rangeT        :: Maybe (Rational, Rational)
                                , rangeX        :: Maybe (Rational, Rational)
                                }

defPlotConfig :: PlotConfig
defPlotConfig = PlotConfig  { outputPath    = "output.png"
                            , sampleNo      = 500
                            , queryAccuracy = Just 32
                            , realAccuracy  = 32
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
            | t1 < t = let ss'' = toList $ insert (t2,x1,x2) ss in (t1, barX ss'') : fillAreas rs' ss''
            | t < t1 = (t, barX ss) : fillAreas rs ss'
            | otherwise = let (_:|ss'') = insert (t2,x1,x2) ss in (t, barX ss'') : fillAreas rs' ss''
          barX ss = (minimum $ map sndOf3 ss, maximum $ map thdOf3 ss)

--hollowPoints :: AlphaColour Double -> [(Double, Double)] -> ...
hollowPoints color vs = plot $ liftEC $ do
    plot_points_style .= hollowCircles 3 1 color
    plot_points_values .= vs

--filledPoints :: AlphaColour Double -> [(Double, Double)] -> ...
filledPoints color vs = plot $ liftEC $ do
    plot_points_style .= filledCircles 3.5 color
    plot_points_values .= vs


getRangeT :: (Fractional a) => Maybe (Rational, Rational) -> CompHybrid a b (Maybe c) -> IO (Maybe (a, a), Maybe c)
getRangeT (Just (l,r)) _  = return (Just (fromRational l, fromRational r), Nothing)
getRangeT Nothing (Ins x) = return (Nothing, x)
getRangeT Nothing (Hyb h)
    = case unroll h of
        Just (d, x) -> return (Just (0, d), x)
        Nothing     -> do
            hPutStrLn stderr "Plotting error: time range not specified for infinite system"
            --hPutStrLn stderr "If your system has infinite iterations, setting an iteration limit will make it finite and possible to plot"
            --of course, if the system has infinite iterations, this case statement
            --will never resolve, so there's no point in showing this error message
            exitWith $ ExitFailure 1

getSampleTs :: (Fractional a) => Integer -> a -> a -> [(Rational, a)]
getSampleTs num ti tf = [let s = i % num in (s, (1 - fromRational s) * ti + fromRational s * tf) | i <- [0..num-1]]

getSamples :: (Fractional a) => (a -> [b]) -> Integer -> Maybe (a,a) -> Maybe b -> [(Rational, a, [b])]
getSamples f num mt mxf = body ++ endp
    where body = maybe [] (\(ti,tf) -> [(s, t, f t) | (s, t) <- getSampleTs num ti tf]) mt
          endp = maybe [] (\xf -> [(1, maybe 0 fst mt, [xf])]) mxf

data Aux = L | M | R deriving (Eq, Ord)
segments :: [(a, [(Step, b)])] -> [(a, (Step, b), (Step, b))] -> [[(a, b)]]
segments evol ds = map (map snd) $ foldr groupify [] $ sortOn fst $ [((i,M),(t,x)) | (t,rr) <- evol, (i,x) <- rr] ++ concat [[((i,R),(t,x)),((j,L),(t,y))] | (t,(i,x),(j,y)) <- ds]
    where groupify x [] = [[x]]
          groupify x@((_,R),_) xss@((((_,L),_):_):_) = [x] : xss
          groupify x@((_,R),_) ((_:xs):xss) = groupify x (xs:xss)
          groupify x (ys:yss) = (x : ys) : yss

toDouble :: (Real a) => a -> Double
toDouble = fromRational . toRational

type S b = (Step, [b])
type Disc a b = (a, Step, Step, [Maybe (b, b)])
class Plottable a b where
    plotHybrid :: PlotConfig -> [String] -> CompHybrid a (S b) (Maybe (S b)) -> [Disc a b] -> IO ()

    default plotHybrid :: (RealFrac a, Real b) => PlotConfig -> [String] -> CompHybrid a (S b) (Maybe (S b)) -> [Disc a b] -> IO ()
    plotHybrid config vars h discs = do
        (mt, xf) <- getRangeT (rangeT config) h
        let samples = getSamples ((`runQuery` queryAccuracy config) . evalCH h) (sampleNo config) mt xf
        let system = transpose [map (t,) $ getZipList $ sequenceA $ fmap (ZipList . sequenceA) xs | (_,t,xs) <- samples]
        let filterDiscs = case mt of
                            Just (ti,tf) -> takeWhile ((<= tf) . fstOf4) . dropWhile ((< ti) . fstOf4)
                            Nothing -> id --all discs are at t=0, so all are valid
        let discsByVar = map catMaybes $ transpose [map (fmap (\(x,y) -> (t,(i,x),(j,y)))) xs | (t,i,j,xs) <- filterDiscs discs]

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
        let n = realAccuracy config
        (mt, xf) <- getRangeT (rangeT config) h
        let boundS = maybe (const (0,0)) (\((til,tir),(tfl,tfr)) s -> ((1-s)*til + s*tfl, (1-s)*tir + s*tfr)) $ fmap (boundDouble n >< boundDouble n) mt
        let approxS = maybe (const 0) (\(ti,tf) s -> (1-s)*ti + s*tf) $ fmap (approxDouble n >< approxDouble n) mt
        let samples = getSamples ((`runQuery` queryAccuracy config) . evalCH h) (sampleNo config) mt xf
        let system = transpose [map (fromRational s,) $ getZipList $ sequenceA $ fmap (ZipList . sequenceA) xs | (s,_,xs) <- samples]
        let filterDiscs = case mt of
                            Just (ti,tf) -> takeWhile (flip (<! tf) n . fstOf4) . dropWhile (flip (<! ti) n . fstOf4)
                            Nothing -> id --all discs are at t=0, so all are valid
        let discsByVar = map catMaybes $ transpose [map (fmap (\(x,y) -> (t,(i,x),(j,y)))) xs | (t,i,j,xs) <- filterDiscs discs]

        toFile def (outputPath config) $ do
            setLayout (rangeT config) (rangeX config)
            sequence_ $ (<$> zip3 vars (system ++ repeat []) (discsByVar ++ repeat [])) $ \(var, evol, ds) -> do
                color <- takeColor
                let segs = segments [(Left t, rr) | (t,rr) <- evol] [(Right t, l, r) | (t,l,r) <- ds]
                joinRects var color [[(either boundS (boundDouble n) t, boundDouble n v) | (t,v) <- vs] | vs <- segs]
                lines var color [[(either approxS (approxDouble n) t, approxDouble n v) | (t,v) <- vs] | vs <- segs]
                hollowPoints color [(approxDouble n t, approxDouble n x) | (t,(_,x),_) <- ds]
                filledPoints color [(approxDouble n t, approxDouble n x) | (t,_,(_,x)) <- ds]