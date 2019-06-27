module LookUpTable where
import Grammar
import Data.List
import Data.Char
import Data.Functor
import Data.Either


generateLutSt :: Statement -> [[(String, Integer, Statement, Integer)]] -> [[(String, Integer, Statement, Integer)]]
generateLutSt s@(SmtDef (VariableDef _ a e)) lut = generateLutEx e newlut
            where n = (getFuncIndex (reverse lut))
                  offset = (calcLocalDataSize n lut) + 1 -- points to the caller's arp
                  newlut = (init lut) ++ [((last lut)) ++ [(a,(offset+1),s,n)]]
generateLutSt s@(SmtDef (FunctionDef _ a _ _)) lut = (init lut) ++ [((last lut)) ++ [(a,0,s,n)]]
            where n = (getFuncIndex (reverse lut))


generateLutSt s@(SmtIf _ _ _) lut = lut ++ [[]]
generateLutSt s@(SmtWhile _ _) lut = lut ++ [[]]
generateLutSt s@(SmtFork _ _ _) lut = lut ++ [[]]

generateLutSt _ lut = lut


generateLutEx (ExprCall s _) lut = lut ++
            [[("#", 0, SmtDef (VariableDef (IntType ()) "#" (ExprConst 0)),  n)]]
            -- get return value from callee
            -- restore previous arp
            where n =  toInteger (length lut)





getFuncIndex ([]:[]) = 0
getFuncIndex ([]:xss) = getFuncIndex xss
getFuncIndex (((_,_,_,n):xs):xss) = n

helpCalcLocalData [] = 0
helpCalcLocalData ([]:xss) = helpCalcLocalData xss
helpCalcLocalData (((_,_,(SmtDef (VariableDef _ _ _)),_):xs):xss) = 1 + helpCalcLocalData (xs:xss)
helpCalcLocalData (((_,_,_,_):xs):xss) = 0 + helpCalcLocalData (xs:xss)

calcLocalDataSize n lut = helpCalcLocalData b
            where (_,b) = splitAt ((fromIntegral n)+1) lut

getStatementFromLut :: Int -> String -> [[(String, Integer, Statement, Integer)]] -> Statement
getStatementFromLut n id lut = helperGetStatement newlut id
            where newlut = reverse (fst (splitAt n lut))

helperGetStatement :: [[(String, Integer, Statement, Integer)]] -> String -> Statement
helperGetStatement ([]:xss) id = helperGetStatement xss id
helperGetStatement (((a,_,c,_):xs):xss) id
 | a == id = c
 | otherwise = helperGetStatement (xs:xss) id