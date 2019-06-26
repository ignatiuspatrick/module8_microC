module Main where

import Frontend
import Text.ParserCombinators.Parsec

main = print("Main!")


parseFile path = testParser <$> readFile path
initFile path = testFront <$> readFile path