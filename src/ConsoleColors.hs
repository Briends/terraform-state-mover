module ConsoleColors where

import System.Console.ANSI (Color (Green, Red, Yellow), ColorIntensity (Vivid), ConsoleLayer (Foreground), SGR (Reset, SetColor), setSGR)

-- Utility for printing colors

deleteColor :: [SGR]
deleteColor = [SetColor Foreground Vivid Red]

addColor :: [SGR]
addColor = [SetColor Foreground Vivid Green]

warnColor :: [SGR]
warnColor = [SetColor Foreground Vivid Yellow]

withColor :: [SGR] -> IO () -> IO ()
withColor color action = do
  setSGR color
  action
  setSGR [Reset]
