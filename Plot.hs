module Plot where

import CompReal
import Lang.Hybrid
import Lang.Interpreter

--using https://hackage.haskell.org/package/Chart
--needs packages Chart and Chart-cairo
import Graphics.Rendering.Chart.Easy hiding (points)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Drawing
import Prelude hiding (lines)
import Control.Applicative (ZipList(..))
import Data.Ratio ((%))
import Data.List (transpose)
import Data.Maybe (isJust, fromJust)


sampleNo :: Integer
sampleNo = 200

samples :: (Fractional a) => a -> [a]
samples tf = map ((tf*) . fromRational . (% sampleNo)) [0..sampleNo]



setLayout = do
    layout_title .= "System Evolution"
    layout_x_axis . laxis_title .= "time"

lineStyle n colour = line_width .~ n
                   $ line_color .~ colour
                   $ def

lines caption color vss = plot $ liftEC $ do
    plot_lines_title .= caption
    plot_lines_style .= lineStyle 1 color
    plot_lines_values .= vss

fillBetween caption color vss = capt >> sequence_ (map fill vss)
    where tcolor = dissolve 0.4 color
          capt = plot $ liftEC $ do
                            plot_fillbetween_style .= solidFillStyle tcolor
                            plot_fillbetween_title .= caption
          fill vs = plot $ liftEC $ do
                                plot_fillbetween_style .= solidFillStyle tcolor
                                plot_fillbetween_values .= vs

points color vs = plot $ liftEC $ do
    plot_points_style .= hollowCircles 3 1 color
    plot_points_values .= vs

candles color vs = plot $ liftEC $ do
    plot_candle_line_style  .= lineStyle 1 color
    plot_candle_width .= 2
    plot_candle_values .= [ Candle t l l 0 u u | (t,(l,u)) <- vs]


fstVal :: RunResult a -> a
fstVal (Val x)    = x
fstVal (Und xs _) = head xs

lastVal :: RunResult a -> a
lastVal (Val x)    = x
lastVal (Und xs _) = last xs

consec :: [a] -> [(a,a)]
consec (x:y:ys) = (x,y) : consec (y:ys)
consec _        = []

isUnd :: RunResult a -> Bool
isUnd (Und _ _) = True
isUnd _         = False

isErr  :: RunResult a -> Bool
isErr (Err _) = True
isErr _       = False

printResult :: (Show a, Show b) => a -> [String] -> RunResult [b] -> IO ()
printResult t vars (Val x) = sequence_ $ putStrLn ("System terminated at t=" ++ show t ++ " with following state:") : [putStrLn (var ++ ": " ++ show val) | (var, val) <- zip vars x]
printResult t vars (Err e) = putStrLn ("System terminated at t=" ++ show t ++ " with error: " ++ e)

lineSegments :: [(a, RunResult (Maybe b))] -> [[(a, b)]]
lineSegments evol = [[fmap (fromJust . lastVal) l, fmap (fromJust . fstVal) r] | (l, r) <- consec evol, isJust $ lastVal $ snd l, not $ isErr $ snd r]

undPoints :: [(a, RunResult (Maybe b))] -> [(a, b)]
undPoints evol = [(t, fromJust x) | (t,rr) <- evol, isUnd rr, x <- allVals rr, isJust x]



toDouble :: (Real a) => a -> Double
toDouble = fromRational . toRational

plotHybrid :: (RealFrac a, Show a, Real b, Show b) => [String] -> Hybrid a (RunResult [Maybe b]) -> IO ()
plotHybrid vars h = do
    let tf = maybe (error "Only finite systems support plotting") id (duration h)
    let system = transpose [map (toDouble t,) $ getZipList $ sequenceA $ fmap ZipList $ eval h t | t <- samples tf]
    
    toFile def "output.png" $ do
        setLayout
        sequence_ $ (<$> zip vars system) $ \(var, evol) -> do
            color <- takeColor
            lines var color $ map (map $ fmap toDouble) $ lineSegments evol
            points color $ map (fmap toDouble) $ undPoints evol

    printResult tf vars (endpoint h)


approxDouble :: (CompReal r) => Int -> r -> Double
approxDouble n r = fromRational $ approx r n

boundDouble :: (CompReal r) => Int -> r -> (Double, Double)
boundDouble n r = let (l, u) = bound r n in (fromRational l, fromRational u)

plotHybridCR :: (CompReal a, Show a, CompReal b, Show b) => [String] -> Hybrid a (RunResult [Maybe b]) -> Int -> IO ()
plotHybridCR vars h n = do
    let tf = maybe (error "Only finite systems support plotting") id (duration h)
    let system = transpose [map (approxDouble n t,) $ getZipList $ sequenceA $ fmap ZipList $ eval h t | t <- samples tf]
    --TODO: time bound?

    toFile def "output.png" $ do
        setLayout
        sequence_ $ (<$> zip vars system) $ \(var, evol) -> do
            color <- takeColor

            fillBetween var color $ (map $ map $ fmap $ boundDouble n) $ lineSegments evol
            lines var color $ map (map $ fmap $ approxDouble n) $ lineSegments evol
            candles color $ map (fmap $ boundDouble n) $ undPoints evol
            

    printResult tf vars (endpoint h)