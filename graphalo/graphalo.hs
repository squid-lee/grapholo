import Control.Monad (unless)
import System.IO (isEOF)

import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import Graphics.Gnuplot.Advanced
import Graphics.Gnuplot.Terminal.X11 as X11

import System.Environment (getArgs)

type LineSplitter = String -> (Double, Double)

argsToLineSplitter :: String -> LineSplitter
argsToLineSplitter s = case map read . words $ s of
     -- pred to make the interface given 1 based
      [ix1, ix2] -> (\s -> (read $ (words s) !! pred ix1, read $ (words s) !! pred ix2))
      _ -> error "Bad field description"


{-
   - one argument, which is one number => plot that number against [1..]
   - one argument, which is a description of fields => select only those
     fields, and plot them against each other
   - two numeric arguments, plot those fields against each other
-}
buildSplitter :: [String] -> LineSplitter
buildSplitter = undefined --

main = do
  args <- getArgs
  let splitter =  if length args == 1 then
                    argsToLineSplitter (head args)
                  else
                    if length args == 0 then
                      argsToLineSplitter "1 2"
                    else
                      argsToLineSplitter (unwords args)

  contents <- getContents

  displayPlot (lines contents) splitter

displayPlot :: [a] -> (a -> (Double, Double)) -> IO ()
displayPlot datas transform = do
  plotSync X11.cons $ Plot2D.parameterFunction Graph2D.points datas transform
  return ()
