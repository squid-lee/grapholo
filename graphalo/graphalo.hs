import Data.Char (isUpper, toLower)
import Data.Monoid (mconcat)
import Data.Maybe (catMaybes)
import Data.Traversable (sequenceA)
import System.Environment (getArgs)

import Graphics.EasyPlot
import Options.Applicative



data Options = Options { humanReadableFields :: Bool
                       , plotTitle :: String
                       , pointStyle :: Style
                       , gnuPlotOptions :: [GnuplotOption]
                       , fields :: [Int]
                       }

defaultOptions = Options False "" Points []

main = do
  options <- execParser $ info cli fullDesc
  let process = buildProcessor options
  contents <- fmap lines getContents
  displayPlot options (process contents)


cli :: Parser Options
cli = Options <$>
        humanReadableFlag <*>
        titleFlag <*>
        plotStyleFlag <*>
        gnuPlotFlags <*>
      -- (many . (argument str)) gives you many stringy arguments
        (fmap (map read) $ many $ argument str (metavar "FIELDS"))
  where
    humanReadableFlag = (switch (mconcat [ long "human-readable"
                                         , short 'h'
                                         , help "treat e.g 1.2K as 1200, 7.45M as 7450000 or 13B as 13"
                                         ]))
    titleFlag = (strOption (mconcat [ long "title"
                                    , short 't'
                                    , help "title for the plot"
                                    , metavar "TITLE"
                                    , value ""
                                    ]))

    gnuPlotFlags = catMaybes <$> sequenceA [interactiveFlag, debugFlag]
      where
        interactiveFlag = flag Nothing (Just Interactive) (mconcat [ long "interactive"
                                                                   , help "Make the plot interactive. BROKEN"
                                                                   ])
        debugFlag = flag Nothing (Just Debug) (mconcat [ long "debug"
                                                       , help "Print gnuplot options"
                                                       ])

    plotStyleFlag = option styleR (mconcat [ long "plot-style"
                                           , short 'p'
                                           , metavar "POINT STYLE"
                                           , help $ unwords ["Style to plot points with. Choose from", unwords styles]
                                           , value Points
                                           ])
      where
        styles = ["Lines", "Points", "Dots", "Impulses", "LinesPoints"]
        styleR :: ReadM Style
        styleR = eitherReader $ \s -> case map toLower s of
                                        "lines" -> Right Lines
                                        "points" -> Right Points
                                        "dots" -> Right Dots
                                        "impulses" -> Right Impulses
                                        "linespoints" -> Right Linespoints
                                        _ -> Left $ unwords ["Unknown Style", s]

buildProcessor :: Options -> ([String] -> [(Double, Double)])
buildProcessor opts = case fields opts of
                        []               ->  zip [1.0..] . map (readValues opts)
                        [field]          ->  zip [1.0..] . map (oneField opts field)
                        [field1, field2] ->  map (twoFields opts field1 field2)
                        args             ->  error $ "Don't know what to do with " ++ (unwords . map show $ args)

oneField :: Options -> Int -> (String -> Double)
oneField = getField

twoFields :: Options -> Int -> Int -> (String -> (Double, Double))
twoFields opts field1 field2 line = (getField opts field1 line, getField opts field2 line)

getField :: Options -> Int -> String -> Double
getField opt field line = if idx < length ws then
                            readValues opt $ ws !! idx
                          else
                            error $ unwords ["Cannot index", concat ["'", line, "'"], "at", show field]
  where
    -- use one based indexing, like awk
    idx = pred field
    ws = words line


readValues :: Options -> (String -> Double)
readValues opts str = case reads str of
                        [] -> readValueError
                        [(val, "")] -> val
                        [(val, suf)] -> if asHuman then interpretSuffix suf $ val else readValueError
                        xs -> error . unwords $ ["Could not unambiguously parse", str, ". Could be ", show xs]
  where
    readValueError = error . unwords $ ["Could not parse", str, "as a number"]
    asHuman = humanReadableFields opts

    interpretSuffix :: String -> (Double -> Double)
    interpretSuffix "B" = (* 1000**0)
    interpretSuffix "K" = (* 1000**1)
    interpretSuffix "M" = (* 1000**2)
    interpretSuffix "G" = (* 1000**3)
    interpretSuffix "T" = (* 1000**4)
    interpretSuffix "P" = (* 1000**5)
    interpretSuffix "E" = (* 1000**6)
    interpretSuffix "Z" = (* 1000**7)
    interpretSuffix "Y" = (* 1000**8)

    interpretSuffix "Bi" = (* 1024**0)
    interpretSuffix "Ki" = (* 1024**1)
    interpretSuffix "Mi" = (* 1024**2)
    interpretSuffix "Gi" = (* 1024**3)
    interpretSuffix "Ti" = (* 1024**4)
    interpretSuffix "Pi" = (* 1024**5)
    interpretSuffix "Ei" = (* 1024**6)
    interpretSuffix "Zi" = (* 1024**7)
    interpretSuffix "Yi" = (* 1024**8)

    interpretSuffix c
      | any isUpper c = interpretSuffix $ map toLower c

displayPlot :: Options -> [(Double, Double)] -> IO Bool
displayPlot opts datas = plot' (gnuPlotOptions opts) X11 $ Data2D [title, style] [] datas
  where
    title = Title $ plotTitle opts
    style = Style $ pointStyle opts
