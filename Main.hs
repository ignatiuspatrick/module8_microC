module Main where

import Frontend
import Grammar
import CodeGen
import Text.ParserCombinators.Parsec
import Sprockell
import Data.List.Split
import Data.Either
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

compile path = compileHelper <$> (initFile path)
compileHelper (Right x) = Right (show (compileProg x [[]] 0 []))
compileHelper (Left err) = (Left err)

compileToFile :: FilePath -> IO (Either String String)
compileToFile path = do
                       instr <- compile path
                       if isLeft instr
                       then return (Left (show (fromLeft "" instr)))
                       else do
                               let str = fromRight "" instr
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
                               contents <- hGetContents stdout
                               return (Right (contents))
                                    where outputPath = compiledFile ++ ".hs"
                                          file = ((splitOn "/" path)!!1)
                                          filename = ((splitOn "." file)!!0)
                                          compiledFile = ("output/") ++ filename

runDebug = runWithDebugger (debuggerSimplePrintAndWait myShow)

samplesWithExp = samplesWithExpTrue ++ samplesWithExpFalse
samplesWithExpTrue = [("samples/advanced.mc", True), ("samples/banking.mc", True), ("samples/basic.mc", True), ("samples/fibonacci.mc", True), ("samples/peterson.mc", True), ("samples/threaded-advanced.mc", True), ("samples/threaded-basic.mc", True)]
samplesWithExpFalse = [("samples/wrong.mc", False)]

testFromFile (path, fail) = do
                        par <- (testFront <$> (readFile path))
                        if isLeft par && (not fail)
                        then return True
                        else do
                            output <- compileToFile path
                            expectedParse <- readFile pathParsed
                            expectedStdout <- readFile pathStdout
                            return ((isRight output) &&
                                    ((show (fromRight (Program []) par)) == expectedParse) &&
                                    ((fromRight "" output) == expectedStdout))
                            where parsed = "-parsed"
                                  stdOut = "-stdout"
                                  testDir = "tests/"
                                  pathParsed = (testDir ++ (splitOn "/" ((((splitOn "." path)!!0)) ++ parsed) !! 1))
                                  pathStdout = (testDir ++ (splitOn "/" ((((splitOn "." path)!!0)) ++ stdOut) !! 1))


testSuite = do
                booleans <- mapM testFromFile samplesWithExp
                return (and booleans)