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
import Data.Maybe (isNothing, fromJust)


sampleNo :: Integer
sampleNo = 200

samples :: (Fractional a) => a -> [a]
samples tf = map ((tf*) . fromRational . (% sampleNo)) [0..sampleNo]


setLayout = do
    layout_title .= "System Evolution"
    layout_x_axis . laxis_title .= "time"

points color vs = liftEC $ do
    plot_points_style .= hollowCircles 2 1 color
    plot_points_values .= vs

lines caption color vs = liftEC $ do
    plot_lines_title .= caption
    plot_lines_style .= LineStyle { _line_width = 1
        , _line_color  = color
        , _line_dashes = []
        , _line_cap    = LineCapButt
        , _line_join   = LineJoinBevel}
    plot_lines_values .= vs

fillBetween caption color vs = liftEC $ do
    plot_fillbetween_title .= caption
    plot_fillbetween_style .= solidFillStyle color
    plot_fillbetween_values .= vs


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


toDouble :: (Real a) => a -> Double
toDouble = fromRational . toRational

plotHybrid :: (RealFrac a, Show a, Real b, Show b) => [String] -> Hybrid a (RunResult [b]) -> IO ()
plotHybrid vars h = do
    let tf = maybe (error "Only finite systems support plotting") id (duration h)
    let system = transpose [map (toDouble t,) $ getZipList $ sequenceA $ fmap ZipList $ eval h t | t <- samples tf]
    
    toFile def "output.png" $ do
        setLayout
        sequence_ $ (<$> zip vars system) $ \(var, evol) -> do
            color <- takeColor
            plot $ lines var color [[fmap (toDouble . lastVal) l, fmap (toDouble . fstVal) r] | (l, r) <- consec evol, not $ isErr $ snd r]
            plot $ points color [(t, toDouble x) | (t,rr) <- evol, isUnd rr, x <- allVals rr]

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
            plot $ lines var color [[fmap (approxDouble n . fromJust . lastVal) l, fmap (approxDouble n . fromJust . fstVal) r] | (l, r) <- consec evol, not $ isErr $ snd r, not $ isNothing $ lastVal $ snd l] 
            --TODO: fillBetween and candles instead of lines
            --plot $ fillBetween var color $ map (fmap boundDouble) evol
            --plot $ line var $ [map (fmap (approxDouble . fst)) evol]

    printResult tf vars (endpoint h)