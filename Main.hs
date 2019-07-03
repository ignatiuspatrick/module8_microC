module Main where

import Frontend
import CodeGen
import Text.ParserCombinators.Parsec
import Sprockell
import Data.List.Split
import Control.Monad
import Debug.Trace
import System.Process

main = print("Main!")


parseFile path = testParser <$> readFile path
initFile path = testFront <$> readFile path

compile path = (\x -> compileProg x [[]] 0 []) <$> (initFile path)

compileToFile path = do
                       str <- getHaskellContents path
                       template <- readFile "output/template"
                       x <- writeFile outputPath (template ++ " " ++  str)
                       y <- callProcess "ghc" ["-w", outputPath]
                       z <- callCommand ("chmod u+x " ++ compiledFile)
                       a <- callCommand ("rm " ++ compiledFile ++ ".o")
                       b <- callCommand ("rm " ++ compiledFile ++ ".hi")
                       c <- callCommand ("rm " ++ compiledFile ++ ".hs")
                       callCommand compiledFile
                            where outputPath = compiledFile ++ ".hs"
                                  file = ((splitOn "/" path)!!1)
                                  filename = ((splitOn "." file)!!0)
                                  compiledFile = ("output/") ++ filename
getHaskellContents path = do
                            instr <- compile path
                            return $ show instr

runDebug = runWithDebugger (debuggerSimplePrintAndWait myShow)

