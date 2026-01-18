{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module PlotBench (plotAccuracy, plotParamMultiI, plotParamMultiAcc) where

import Utils

--needs packages Chart and Chart-cairo
import Graphics.Rendering.Chart.Easy hiding (points, both)
import Graphics.Rendering.Chart.Backend.Cairo
import Prelude hiding (lines)
import Control.Monad (unless)

--takes a number from 0 to 1 and outputs a color
gradient x | y < 2     = blend y lime cyan
           | y < 4     = blend ((y-2)/2) yellow lime
           | otherwise = blend ((y-4)/3) red yellow
    where y = 7 * x


lineStyle n color = line_width .~ n
                    $ line_color .~ color
                    $ def

lines color vss = plot $ liftEC $ do
    plot_lines_style .= lineStyle 1 color
    plot_lines_values .= vss

points caption color vs = plot $ liftEC $ do
    plot_points_title .= caption
    plot_points_style .= filledCircles 3 color
    plot_points_values .= vs



plotAux xs = sequence_ $ (<$> xs) $ \(name, vals) -> unless (null vals) $ do
    color <- takeColor
    lines color [vals]
    points name color vals

--plots the benchmarked time that each CR library took to approximate a number to some accuracy
plotAccuracy :: String -> String -> [(String, [(Int, Double)])] -> IO ()
plotAccuracy path title xs = do
    toFile def (path ++ title ++ ".png") $ do
        layout_title .= title
        layout_x_axis . laxis_title .= "accuracy"
        layout_y_axis . laxis_title .= "time (ms)"

        let maxTime = maximum $ concat $ map (map snd . snd) xs
        layout_y_axis . laxis_generate .= scaledAxis def (0, maxTime)

        setColors $ opaque <$> [blue,green,red,orange,cyan]
        plotAux xs

--plots the benchmarked time versus some parameter for each implementation
plotParamMultiI :: String -> String -> [(String, [(Rational, Double)])] -> IO ()
plotParamMultiI path title xs = do
    toFile def (path ++ title ++ ".png") $ do
        layout_title .= title
        layout_x_axis . laxis_title .= "parameter"
        layout_y_axis . laxis_title .= "time (ms)"

        let maxTime = maximum $ concat $ map (map snd . snd) xs
        layout_y_axis . laxis_generate .= scaledAxis def (0, maxTime)

        setColors $ opaque <$> [blue,green,red,orange,cyan]
        plotAux $ map (id >< map ((fromRational :: Rational -> Double) >< id)) xs

--plots the benchmarked time versus some parameter for each accuracy
plotParamMultiAcc :: String -> String -> [(Int, [(Rational, Double)])] -> IO ()
plotParamMultiAcc path title xs = do
    toFile def (path ++ title ++ ".png") $ do
        layout_title .= title
        layout_x_axis . laxis_title .= "parameter"
        layout_y_axis . laxis_title .= "time (ms)"

        let maxTime = maximum $ concat $ map (map snd . snd) xs
        layout_y_axis . laxis_generate .= scaledAxis def (0, maxTime)

        let minAcc = minimum $ map fst xs
        let divAcc = fromIntegral $ max 1 $ maximum (map fst xs) - minAcc
        setColors $ opaque . darken 0.8 . gradient . (/divAcc) . fromIntegral . subtract minAcc . fst <$> xs
        
        plotAux $ map (show >< map ((fromRational :: Rational -> Double) >< id)) xs