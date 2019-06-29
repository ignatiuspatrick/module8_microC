module LookUpTable where
import Grammar
import Data.List
import Data.Char
import Data.Functor
import Data.Either
import Debug.Trace

generateLutSt :: Statement -> [[(String, Integer, Statement, Integer)]] -> [[(String, Integer, Statement, Integer)]]
generateLutSt s@(SmtDef (VariableDef _ a e)) lut = newlut
            where n = (getFuncIndex (reverse lut))
                  offset = (calcLocalDataSize n lut) + 1 -- points to the caller's arp
                  newlut = (init lut) ++ [((last lut)) ++ [(a,offset,s,n)]]

generateLutSt s@(SmtDef (FunctionDef _ a _ _)) lut = (init lut) ++ [((last lut)) ++ [(a,0,s,n)]]
            where n = (getFuncIndex (reverse lut))


generateLutSt s@(SmtCall _ _) lut = lut ++ [[("#", 0, SmtDef (FunctionDef (IntType ()) "#" [(Param (IntType()) "&")] []),  n)]]
             where  n = toInteger (length lut)

generateLutSt s@(SmtIf _ _ _) lut = lut ++ [[]]
generateLutSt s@(SmtWhile _ _) lut = lut ++ [[]]
generateLutSt s@(SmtFork _ _ _) lut = lut ++ [[]]

generateLutSt _ lut = lut


fixUpLut (ExprCall s _) lut = (init lut)

generateLutEx (ExprCall s _) lut = lut ++
            [[("#", 0, SmtDef (FunctionDef (IntType ()) "#" [(Param (IntType()) "&")] []),  n)]]
            where n =  toInteger (length lut)

generateLutEx _ lut = lut




getFuncIndex ([]:[]) = 0
getFuncIndex ([]:xss) = getFuncIndex xss
getFuncIndex (((_,_,_,n):xs):xss) = n

helpCalcLocalData [] = 0
helpCalcLocalData ([]:xss) = helpCalcLocalData xss
helpCalcLocalData (((_,_,(SmtDef (VariableDef _ _ _)),_):xs):xss) = 1 + helpCalcLocalData (xs:xss)
helpCalcLocalData (((_,_,_,_):xs):xss) = 0 + helpCalcLocalData (xs:xss)

calcLocalDataSize n lut = helpCalcLocalData b
            where (_,b) = splitAt (fromIntegral n) lut

getStatementFromLut :: Int -> String -> [[(String, Integer, Statement, Integer)]] -> Statement
getStatementFromLut n id lut = helperGetStatement newlut id
            where newlut = reverse (fst (splitAt (n+1) lut))


helperGetStatement :: [[(String, Integer, Statement, Integer)]] -> String -> Statement
helperGetStatement [] id = error("helperGetStatement can't find " ++ id)
helperGetStatement ([]:xss) id = helperGetStatement xss id
helperGetStatement (((a,_,c,_):xs):xss) id
 | a == id = c
 | otherwise = helperGetStatement (xs:xss) id