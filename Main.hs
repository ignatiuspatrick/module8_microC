module Main where

import Frontend
import Grammar
import CodeGen
import Text.ParserCombinators.Parsec
import Sprockell
import Data.List.Split
import Control.Monad
import Debug.Trace
import System.Process
import System.IO
import Control.Exception

main = print("Main!")


parseFile path = testParser <$> readFile path
initFile path = do
                    str <- testFront <$> (readFile path)
                    return str

compile path = (\x -> compileProg x [[]] 0 []) <$> (initFile path)

compileToFile path = do
                       str <- getHaskellContents path
                       template <- readFile "output/template"
                       writeFile outputPath (template ++ " " ++  str)
                       (_, Just bla, _, comp) <- createProcess (proc "ghc" [outputPath]) {std_out = CreatePipe, std_err = CreatePipe}
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

-- "samples/test.mc",
samples = ["samples/advanced.mc", "samples/banking.mc", "samples/basic.mc", "samples/fibonacci.mc", "samples/peterson.mc", "samples/threaded-advanced.mc", "samples/threaded-basic.mc"]

writeToFile path = do
                            ins <- compileToFile path
                            program <- initFile path
                            writeFile pathParsed (show program)
                            writeFile pathStdout (show ins)
                            where parsed = "-parsed"
                                  stdOut = "-stdout"
                                  testDir = "tests/"
                                  pathParsed = (testDir ++ (splitOn "/" ((((splitOn "." path)!!0)) ++ parsed) !! 1))
                                  pathStdout = (testDir ++ (splitOn "/" ((((splitOn "." path)!!0)) ++ stdOut) !! 1))

errFunc e = do
                let err = show (e :: SomeException)
                return (Program [])

testFromFile path = do
                        output <- compileToFile path
                        par <- fromRight (show (testFront <$> (readFile path))) "error"
                        expectedParse <- readFile pathParsed
                        expectedStdout <- readFile pathStdout
                        return (output == expectedStdout)
                        where parsed = "-parsed"
                              stdOut = "-stdout"
                              testDir = "tests/"
                              pathParsed = (testDir ++ (splitOn "/" ((((splitOn "." path)!!0)) ++ parsed) !! 1))
                              pathStdout = (testDir ++ (splitOn "/" ((((splitOn "." path)!!0)) ++ stdOut) !! 1))


testSuite = do
                        booleans <- mapM testFromFile samples
                        return (and booleans)