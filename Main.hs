module Main where

import Frontend
import CodeGen
import Text.ParserCombinators.Parsec
import Sprockell
import Data.List.Split
import Control.Monad
import Debug.Trace
import System.Process
import System.IO

main = print("Main!")


parseFile path = testParser <$> readFile path
initFile path = testFront <$> readFile path

compile path = (\x -> compileProg x [[]] 0 []) <$> (initFile path)

compileToFile path = do
                       str <- getHaskellContents path
                       template <- readFile "output/template"
                       writeFile outputPath (template ++ " " ++  str)
                       (_, Just bla, _, comp) <- createProcess (proc "ghc" [outputPath]) {std_out = CreatePipe}
                       waitForProcess comp
                       callCommand ("chmod u+x " ++ compiledFile)
                       callCommand ("rm " ++ compiledFile ++ ".o")
                       callCommand ("rm " ++ compiledFile ++ ".hi")
                       callCommand ("rm " ++ compiledFile ++ ".hs")
                       (_, Just stdout, _, cp) <- createProcess (proc compiledFile []) {std_out = CreatePipe}
                       waitForProcess cp
                       hGetContents stdout
                            where outputPath = compiledFile ++ ".hs"
                                  file = ((splitOn "/" path)!!1)
                                  filename = ((splitOn "." file)!!0)
                                  compiledFile = ("output/") ++ filename
getHaskellContents path = do
                            instr <- compile path
                            return $ show instr

runDebug = runWithDebugger (debuggerSimplePrintAndWait myShow)


testFromFile path = do
                        output <- compileToFile path
                        return output