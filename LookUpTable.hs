module LookUpTable where
import Grammar
import Data.List
import Data.Char
import Data.Functor
import Data.Either
import Debug.Trace


type ID = String
type Offset = Integer
type FunctionIndexInLUT = Integer

type Entry = (ID, Offset, Statement, FunctionIndexInLUT)
type LookUpScope = [Entry]
type LookUpTable = [LookUpScope]

generateLutSt :: Statement -> LookUpTable -> LookUpTable
generateLutSt s@(SmtDef (VariableDef _ a e)) lut = newlut
            where n = (getFuncIndex (reverse lut))
                  offset = (calcLocalDataSize n lut) + 1 -- points to the caller's arp
                  newlut = (init lut) ++ [((last lut)) ++ [(a,offset,s,n)]]

generateLutSt s@(SmtDef (FunctionDef _ a _ _)) lut = (init lut) ++ [((last lut)) ++ [(a,0,s,n)]]
            where n = (getFuncIndex (reverse lut))


generateLutSt (SmtCall s exprs) lut = lut ++ [defs]
             where  n = toInteger (length lut)
                    dummy = [("#", 0, SmtDef (FunctionDef (IntType ()) "#" [(Param (IntType()) "&")] []),  n)]
                    paramDefs = generateDefsForParams exprs params lut n
                    defs = dummy ++ paramDefs
                    (SmtDef (FunctionDef _ _ params _)) = getStatementFromLut  ((fromIntegral n) - 1) s lut

generateLutSt s@(SmtIf _ _ _) lut = lut ++ [[]]
generateLutSt s@(SmtWhile _ _) lut = lut ++ [[]]
generateLutSt s@(SmtFork _ _ _) lut = lut ++ [[]]

generateLutSt _ lut = lut


fixUpLut (ExprCall s _) lut = (init lut)

generateLutEx (ExprCall s exprs) lut = lut ++ [defs]
            where n =  toInteger (length lut)
                  dummy = [("#", 0, SmtDef (FunctionDef (IntType ()) "#" [(Param (IntType()) "&")] []),  n)]
                  paramDefs = generateDefsForParams exprs params lut n
                  defs = dummy ++ paramDefs
                  (SmtDef (FunctionDef _ _ params _)) = getStatementFromLut  ((fromIntegral n) - 1) s lut

generateLutEx _ lut = lut

generateDefsForParams :: [Expression] -> [Param] -> LookUpTable -> Integer -> LookUpScope
generateDefsForParams [] [] lut n = []
generateDefsForParams (e:exprs) p@((Param a id):params) lut n = (id, offset, (SmtDef (VariableDef a id e)), n) : generateDefsForParams exprs params lut n
        where offset = toInteger (negate (2 + (length p)))



getFuncIndex ([]:[]) = 0
getFuncIndex ([]:xss) = getFuncIndex xss
getFuncIndex (((_,_,_,n):xs):xss) = n

helpCalcLocalData [] = 0
helpCalcLocalData ([]:xss) = helpCalcLocalData xss
helpCalcLocalData (((_,_,(SmtDef (VariableDef _ _ _)),_):xs):xss) = 1 + helpCalcLocalData (xs:xss)
helpCalcLocalData (((_,_,_,_):xs):xss) = 0 + helpCalcLocalData (xs:xss)

calcLocalDataSize n lut = helpCalcLocalData b
            where (_,b) = splitAt (fromIntegral n) lut

getStatementFromLut :: Int -> String -> LookUpTable -> Statement
getStatementFromLut n id lut = helperGetStatement newlut id
            where newlut = reverse (fst (splitAt (n+1) lut))


helperGetStatement :: LookUpTable -> String -> Statement
helperGetStatement [] id = error("helperGetStatement can't find " ++ id)
helperGetStatement ([]:xss) id = helperGetStatement xss id
helperGetStatement (((a,_,c,_):xs):xss) id
 | a == id = c
 | otherwise = helperGetStatement (xs:xss) id


spaceInSharedMemForVar = 3

movetToShared (SmtFork ids s1 s2) lut arp shared threadNo = prepToMoveIntoShared ids lut arp shared threadNo
movetToShared _ _ _ shared threadNo = shared

prepToMoveIntoShared [] lut arp shared threadNo = shared
prepToMoveIntoShared ((ExprVar id):exprs) lut arp shared threadNo =
        if and (map (\(x, _) -> x /= id) shared)
        then prepToMoveIntoShared exprs lut arp ((id, (((length shared) * spaceInSharedMemForVar) + threadNo)) : shared) threadNo
        else prepToMoveIntoShared exprs lut arp shared threadNo