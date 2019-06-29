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

compile path = (\x -> compileProg x [[]] 0 ) <$> (initFile path)

runProg path = run <$> (compile path)

prog :: [[Instruction]]
prog = [[Load (ImmValue 0) 7,Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 1),Load (ImmValue 10) 2,Push 2,Pop 2,Store 2 (DirAddr 2),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 3),Load (ImmValue 2) 2,Push 2,Load (ImmValue 8) 7,Store 7 (ImmValue 0),Pop 2,Load (ImmValue 3) 3,Compute Sub 7 3 3,Store 2 (IndAddr 3),Load (ImmValue 1) 2,Push 2,Pop 2,Store 2 (DirAddr 9),Load (ImmValue 0) 2,Push 2,Pop 2,Store 2 (DirAddr 10),Compute Add 7 0 6,Load (ImmValue 2) 4,Compute Add 4 6 6,Load (IndAddr 6) 2,Push 2,Pop 2,Store 2 (DirAddr 6),Load (DirAddr 6) 2,Push 2,Load (ImmValue 0) 7,Pop 2,Store 2 (DirAddr 4),WriteInstr 2 (DirAddr 65536),EndProg]]


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

