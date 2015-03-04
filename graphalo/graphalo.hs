import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import Graphics.Gnuplot.Advanced
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

import System.Environment (getArgs)
import Options.Applicative
import Data.Monoid (mconcat)

data Options = Options {
  humanReadableFields :: Bool,
  fields :: [Int]
}
  deriving Show


main = do
  options <- execParser $ info cli fullDesc
  let process = buildProcessor (fields options)
  contents <- fmap lines getContents
  displayPlot (process contents)


cli :: Parser Options
cli = Options <$>
      -- switch is boolean argument
      switch (mconcat [ long "parse-si-suffix-fields"
                      , short 's'
                      , help "treat e.g 1.2K as 1200, 7.45M as 7450000 or 13B as 13"
                      ])  <*>
      -- (many . (argument str)) gives you many stringy arguments
      (fmap (map read) $ many $ argument str (metavar "FIELDS"))


buildProcessor :: [Int] -> ([String] -> [(Double, Double)])
buildProcessor [] = zip [1.0..] . map read
buildProcessor [field] = zip [1.0..] . map (oneField field)
buildProcessor [field1, field2] = map (twoFields field1 field2)
buildProcessor args = error $ "Don't know what to do with " ++ (unwords . map show $ args)

oneField :: Int -> (String -> Double)
oneField = getField

twoFields :: Int -> Int -> (String -> (Double, Double))
twoFields field1 field2 line = (getField field1 line, getField field2 line)

getField :: Int -> String -> Double
getField field line = if idx < length ws then
                        read $ ws !! idx
                      else
                        error $ unwords ["Cannot index", line, "at", show field]
  where
    -- use one based indexing, like awk
    idx = pred field
    ws = words line


displayPlot :: [(Double, Double)] -> IO ()
displayPlot datas = do
  plotSync DefaultTerm.cons $ Plot2D.parameterFunction Graph2D.points datas id
  return ()
