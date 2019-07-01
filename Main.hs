module Main where

import Frontend
import CodeGen
import Text.ParserCombinators.Parsec
import Sprockell
import Data.List.Split
import Control.Monad
import Debug.Trace


main = print("Main!")


parseFile path = testParser <$> readFile path
initFile path = testFront <$> readFile path

compile path = (\x -> compileProg x [[]] 0 []) <$> (initFile path)

runProg path = run <$> (compile path)

-- prog :: [[Instruction]]
progBasic = [[Load (ImmValue 0) 7,Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 1),Load (ImmValue 10) 2,Push 2,Pop 2,Store 2 (DirAddr 2),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 3),Load (ImmValue 2) 2,Push 2,Load (ImmValue 8) 7,Store 7 (ImmValue 0),Pop 2,Load (ImmValue 3) 3,Compute Sub 7 3 3,Store 2 (IndAddr 3),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 9),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 10),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Pop 2,Store 2 (DirAddr 6),Load (DirAddr 6) 2,Push 2,Load (ImmValue 0) 7,Pop 2,Store 2 (DirAddr 4),WriteInstr 2 (DirAddr 65536),EndProg]]

progAdvance = [[Load (ImmValue 0) 7,Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 1),Load (ImmValue 10) 2,Push 2,Pop 2,Store 2 (DirAddr 2),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 3),Compute Add 7 0 6,Load (ImmValue 3) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Pop 2,Branch 2 (Rel 2),Jump (Rel 95),Load (ImmValue 7) 7,Store 7 (ImmValue 0),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 8),Compute Add 7 0 6,Load (ImmValue 1) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Load (ImmValue 1) 2,Push 2,Pop 2,Pop 3,Compute Equal 2 3 4,Push 4,Pop 2,Branch 2 (Rel 2),Jump (Rel 5),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 9),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 9),Compute Add 7 0 6,Load (IndAddr 6) 6,Load (ImmValue 3) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Load (ImmValue 1) 2,Push 2,Pop 2,Pop 3,Compute Equal 2 3 4,Push 4,Pop 2,Branch 2 (Rel 2),Jump (Rel 40),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Load (ImmValue 4) 2,Push 2,Pop 2,Pop 3,Compute Add 2 3 4,Push 4,Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Pop 2,Store 2 (IndAddr 6),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Load (ImmValue 10) 2,Push 2,Pop 2,Pop 3,Compute GtE 2 3 4,Push 4,Pop 2,Branch 2 (Rel 2),Jump (Rel 9),Load (ImmValue 1) 2,Push 2,Compute Add 7 0 6,Load (IndAddr 6) 6,Load (ImmValue 3) 4,Compute Add 4 6 6,Pop 2,Store 2 (IndAddr 6),Jump (Rel (-40)),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Pop 2,Store 2 (DirAddr 5),Load (DirAddr 5) 2,Push 2,Load (ImmValue 0) 7,Pop 2,Store 2 (DirAddr 4),WriteInstr 2 (DirAddr 65536),EndProg]]

progThreaded = [[Load (ImmValue 0) 7,Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 1),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 2),Load (ImmValue 6) 7,Store 7 (ImmValue 0),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 7),Compute Add 7 0 6,Load (ImmValue 1) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,WriteInstr 2 (DirAddr 1),WriteInstr 0 (DirAddr 2),WriteInstr 0 (DirAddr 3),Compute Add 7 0 6,Load (IndAddr 6) 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,WriteInstr 2 (DirAddr 4),WriteInstr 0 (DirAddr 5),WriteInstr 0 (DirAddr 6),WriteInstr 0 (DirAddr 0),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 8),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Load (ImmValue 1) 2,Push 2,Pop 2,Pop 3,Compute Equal 2 3 4,Push 4,Pop 2,Branch 2 (Rel 2),Jump (Rel 5),Load (ImmValue 50) 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 1),TestAndSet (DirAddr 0),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),Compute Add 7 0 6,Load (ImmValue 1) 4,Compute Add 4 6 6,ReadInstr (DirAddr 1),Receive 2,Store 2 (IndAddr 6),Compute Add 7 0 6,Load (IndAddr 6) 6,Load (ImmValue 2) 4,Compute Add 4 6 6,ReadInstr (DirAddr 4),Receive 2,Store 2 (IndAddr 6),Compute Add 7 0 6,Load (ImmValue 1) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Pop 2,Store 2 (DirAddr 4),Load (DirAddr 4) 2,Push 2,Load (ImmValue 0) 7,Pop 2,Store 2 (DirAddr 3),WriteInstr 2 (DirAddr 65536),EndProg],[TestAndSet (DirAddr 0),Receive 2,Branch 2 (Rel 2),Jump (Rel (-3)),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 8),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Load (ImmValue 0) 2,Push 2,Pop 2,Pop 3,Compute Or 2 3 4,Push 4,Pop 2,Branch 2 (Rel 2),Jump (Rel 5),Load (ImmValue 30) 2,Push 2,Pop 2,WriteInstr 2 (DirAddr 4),WriteInstr 0 (DirAddr 0),WriteInstr 2 (DirAddr 65536),EndProg]]









-- compileToFile path = do
--                         str <- getHaskellContents path
--                         template <- readFile "output/template"
--                         progStr <- liftM2 (++) template str
--                         writeFile outputPath progStr
--                             where outputPath = ("output/") ++ filename
--                                   file = ((splitOn "/" path)!!1)
--                                   filename = ((splitOn "." file)!!0)
--
-- getHaskellContents path = do (show <$> (compile path))

