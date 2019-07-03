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
--                        c <- callCommand ("rm " ++ compiledFile ++ ".hs")
                       callCommand compiledFile
                            where outputPath = compiledFile ++ ".hs"
                                  file = ((splitOn "/" path)!!1)
                                  filename = ((splitOn "." file)!!0)
                                  compiledFile = ("output/") ++ filename
getHaskellContents path = do
                            instr <- compile path
                            return $ show instr

runDebug = runWithDebugger (debuggerSimplePrintAndWait myShow)


progT = [[Load (ImmValue 0) 7,Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 1),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 2),Load (ImmValue 6) 7,Store 7 (ImmValue 0),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 7),Compute Add 7 0 6,Load (IndAddr 6) 6,Load (ImmValue 1) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,WriteInstr 2 (DirAddr 1),WriteInstr 0 (DirAddr 2),WriteInstr 0 (DirAddr 3),Compute Add 7 0 6,Load (IndAddr 6) 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,WriteInstr 2 (DirAddr 4),WriteInstr 0 (DirAddr 5),WriteInstr 0 (DirAddr 6),WriteInstr 0 (DirAddr 7),WriteInstr 0 (DirAddr 8),ReadInstr (DirAddr 7),Receive 2,Load (ImmValue (-1)) 4,Compute Equal 2 4 2,Branch 2 (Rel 2),Jump (Rel (-5)),ReadInstr (DirAddr 8),Receive 2,Load (ImmValue (-1)) 4,Compute Equal 2 4 2,Branch 2 (Rel 2),Jump (Rel (-5)),Compute Add 7 0 6,Load (IndAddr 6) 6,Load (ImmValue 1) 4,Compute Add 4 6 6,ReadInstr (DirAddr 1),Receive 2,Store 2 (IndAddr 6),Compute Add 7 0 6,Load (IndAddr 6) 6,Load (ImmValue 2) 4,Compute Add 4 6 6,ReadInstr (DirAddr 4),Receive 2,Store 2 (IndAddr 6),Compute Add 7 0 6,Load (IndAddr 6) 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Pop 2,Store 2 (DirAddr 4),Load (DirAddr 4) 2,Push 2,Load (ImmValue 0) 7,Pop 2,Store 2 (DirAddr 3),WriteInstr 2 (DirAddr 65536),EndProg],[TestAndSet (DirAddr 7),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),Load (ImmValue 33) 2,Push 2,Pop 2,Store 2 (DirAddr 8),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 9),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,WriteInstr 2 (DirAddr 9),WriteInstr 0 (DirAddr 10),WriteInstr 0 (DirAddr 11),Compute Add 7 0 6,Load (ImmValue 3) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,WriteInstr 2 (DirAddr 12),WriteInstr 0 (DirAddr 13),WriteInstr 0 (DirAddr 14),WriteInstr 0 (DirAddr 15),WriteInstr 0 (DirAddr 16),ReadInstr (DirAddr 15),Receive 2,Load (ImmValue (-1)) 4,Compute Equal 2 4 2,Branch 2 (Rel 2),Jump (Rel (-5)),ReadInstr (DirAddr 16),Receive 2,Load (ImmValue (-1)) 4,Compute Equal 2 4 2,Branch 2 (Rel 2),Jump (Rel (-5)),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,ReadInstr (DirAddr 9),Receive 2,Store 2 (IndAddr 6),Compute Add 7 0 6,Load (ImmValue 3) 4,Compute Add 4 6 6,ReadInstr (DirAddr 12),Receive 2,Store 2 (IndAddr 6),Load (ImmValue (-1)) 4,WriteInstr 4 (DirAddr 7),WriteInstr 2 (DirAddr 65536),EndProg],[TestAndSet (DirAddr 8),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 8),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 9),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Load (ImmValue 1000) 2,Push 2,Pop 2,Pop 3,Compute Lt 3 2 4,Push 4,Pop 2,Branch 2 (Rel 2),Jump (Rel 47),ReadInstr (DirAddr 5),Receive 2,Branch 2 (Rel 2),Jump (Rel 6),ReadInstr (DirAddr 6),Receive 2,Compute Equal 2 1 2,Branch 2 (Rel 2),Jump (Rel (-8)),ReadInstr (DirAddr 4),Receive 2,Push 2,Load (ImmValue 1) 2,Push 2,Pop 2,Pop 3,Compute Add 3 2 4,Push 4,Pop 4,ReadInstr (DirAddr 5),Receive 2,Branch 2 (Rel 2),Jump (Rel 6),ReadInstr (DirAddr 6),Receive 2,Compute Equal 2 1 2,Branch 2 (Rel 2),Jump (Rel (-8)),WriteInstr 4 (DirAddr 4),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Load (ImmValue 1) 2,Push 2,Pop 2,Pop 3,Compute Add 3 2 4,Push 4,Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Pop 2,Store 2 (IndAddr 6),Jump (Rel (-59)),Load (ImmValue (-1)) 4,WriteInstr 4 (DirAddr 8),WriteInstr 2 (DirAddr 65536),EndProg],[TestAndSet (DirAddr 15),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),TestAndSet (DirAddr 10),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),WriteInstr 1 (DirAddr 11),Load (ImmValue 10) 2,Push 2,Pop 4,ReadInstr (DirAddr 10),Receive 2,Branch 2 (Rel 2),Jump (Rel 6),ReadInstr (DirAddr 11),Receive 2,Compute Equal 2 1 2,Branch 2 (Rel 2),Jump (Rel (-8)),WriteInstr 4 (DirAddr 9),WriteInstr 0 (DirAddr 10),Load (ImmValue (-1)) 2,WriteInstr 2 (DirAddr 11),Load (ImmValue (-1)) 4,WriteInstr 4 (DirAddr 15),WriteInstr 2 (DirAddr 65536),EndProg],[TestAndSet (DirAddr 16),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),TestAndSet (DirAddr 10),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),WriteInstr 1 (DirAddr 11),Load (ImmValue 50) 2,Push 2,Pop 4,ReadInstr (DirAddr 10),Receive 2,Branch 2 (Rel 2),Jump (Rel 6),ReadInstr (DirAddr 11),Receive 2,Compute Equal 2 1 2,Branch 2 (Rel 2),Jump (Rel (-8)),WriteInstr 4 (DirAddr 9),WriteInstr 0 (DirAddr 10),Load (ImmValue (-1)) 2,WriteInstr 2 (DirAddr 11),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 10),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,WriteInstr 2 (DirAddr 9),WriteInstr 0 (DirAddr 10),WriteInstr 0 (DirAddr 11),Compute Add 7 0 6,Load (ImmValue 4) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,WriteInstr 2 (DirAddr 17),WriteInstr 0 (DirAddr 18),WriteInstr 0 (DirAddr 19),WriteInstr 0 (DirAddr 20),WriteInstr 0 (DirAddr 21),ReadInstr (DirAddr 20),Receive 2,Load (ImmValue (-1)) 4,Compute Equal 2 4 2,Branch 2 (Rel 2),Jump (Rel (-5)),ReadInstr (DirAddr 21),Receive 2,Load (ImmValue (-1)) 4,Compute Equal 2 4 2,Branch 2 (Rel 2),Jump (Rel (-5)),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,ReadInstr (DirAddr 9),Receive 2,Store 2 (IndAddr 6),Compute Add 7 0 6,Load (ImmValue 4) 4,Compute Add 4 6 6,ReadInstr (DirAddr 17),Receive 2,Store 2 (IndAddr 6),Load (ImmValue (-1)) 4,WriteInstr 4 (DirAddr 16),WriteInstr 2 (DirAddr 65536),EndProg],[TestAndSet (DirAddr 20),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),TestAndSet (DirAddr 10),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),WriteInstr 1 (DirAddr 11),Load (ImmValue 10) 2,Push 2,Pop 4,ReadInstr (DirAddr 10),Receive 2,Branch 2 (Rel 2),Jump (Rel 6),ReadInstr (DirAddr 11),Receive 2,Compute Equal 2 1 2,Branch 2 (Rel 2),Jump (Rel (-8)),WriteInstr 4 (DirAddr 9),WriteInstr 0 (DirAddr 10),Load (ImmValue (-1)) 2,WriteInstr 2 (DirAddr 11),Load (ImmValue (-1)) 4,WriteInstr 4 (DirAddr 20),WriteInstr 2 (DirAddr 65536),EndProg],[TestAndSet (DirAddr 21),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),TestAndSet (DirAddr 10),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),WriteInstr 1 (DirAddr 11),Load (ImmValue 50) 2,Push 2,Pop 4,ReadInstr (DirAddr 10),Receive 2,Branch 2 (Rel 2),Jump (Rel 6),ReadInstr (DirAddr 11),Receive 2,Compute Equal 2 1 2,Branch 2 (Rel 2),Jump (Rel (-8)),WriteInstr 4 (DirAddr 9),WriteInstr 0 (DirAddr 10),Load (ImmValue (-1)) 2,WriteInstr 2 (DirAddr 11),Load (ImmValue (-1)) 4,WriteInstr 4 (DirAddr 21),WriteInstr 2 (DirAddr 65536),EndProg]]




