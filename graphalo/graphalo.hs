import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import Graphics.Gnuplot.Advanced
import Graphics.Gnuplot.Terminal.X11 as X11

import System.Environment (getArgs)

main = do
  args <- getArgs
  let process = buildProcessor (map read args)

  contents <- fmap lines getContents

  displayPlot (process contents)

buildProcessor :: [Int] -> ([String] -> [(Double, Double)])
buildProcessor [] = zip [1.0..] . map read
buildProcessor [field] = zip [1.0..] . map (oneField field)
buildProcessor [field1, field2] = map (twoFields field1 field2)
buildProcessor args = error $ "Don't know what to do with " ++ (unwords . map show $ args)

oneField :: Int -> (String -> Double)
oneField field line = read (ws !! idx)
  where
    -- use one based indexing, like awk
    idx = pred field
    ws = words line

twoFields :: Int -> Int -> (String -> (Double, Double))
twoFields field1 field2 line = (getField field1 line, getField field2 line)
  where
    -- use one based indexing, like awk
    getField field line = read $ ws !! idx
      where
        ws = words line
        idx = pred field


displayPlot :: [(Double, Double)] -> IO ()
displayPlot datas = do
  plotSync X11.cons $ Plot2D.parameterFunction Graph2D.points datas id
  return ()
