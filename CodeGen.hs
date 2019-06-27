module CodeGen where

import Sprockell
import Frontend
import Grammar
import Text.ParserCombinators.Parsec


-- since a code is for one time, then one lut is for one piece of code
-- the construction is from top to bottom, execution refers to the called function

getCmpOp (OrderLT _) = Lt
getCmpOp (OrderLE _) = LtE
getCmpOp (OrderEQ _) = Equal
getCmpOp (OrderNE _) = NEq
getCmpOp (OrderGT _) = Gt
getCmpOp (OrderGE _) = GtE

getBinOp (BinaryAnd _) = And
getBinOp (BinaryOr _) = Or

getOffsetById id ([]:xss) = getOffsetById id xss
getOffsetById id (((a,b,c):xs):xss)
    | a == id = b
    | otherwise = getOffsetById id (xs:xss)

compileExpr :: Expression -> [[(String, Integer, Statement, Integer)]] -> [Instruction]
compileExpr (ExprConst a) lut =
        [
                Load (ImmValue (fromIntegral a)) regA -- load a into register 0
                , Push regA -- push into register A
        ]

-- for true put 1, and for false put 0
compileExpr (ExprTrue _) lut =
        [
                Load (ImmValue (intBool True)) regA
                , Push regA
        ]

compileExpr (ExprFalse _) lut =
        [
                Load (ImmValue (intBool False)) regA
                , Push regA
        ]

-- -- find id in lut, get offset, get arp, add offset to arp, load result -> regA
compileExpr (ExprVar xs) lut =
        [
            Load (DirAddr (fromIntegral offset)) regA -- change a later with the offset
            , Push regA
        ]
        where offset = getOffsetById xs (reverse lut)


compileExpr (ExprAdd a b) lut = (compileExpr a lut) ++ (compileExpr b lut) ++
        [
            Pop regA
            , Pop regB
            , Compute Add regA regB regC
            , Push regC
        ]

compileExpr (ExprSubtract a b) lut = (compileExpr a lut) ++ (compileExpr b lut) ++
        [
            Pop regA
            , Pop regB
            , Compute Sub regA regB regC
            , Push regC
        ]

compileExpr (ExprMult a b) lut = (compileExpr a lut) ++ (compileExpr b lut) ++
        [
            Pop regA
            , Pop regB
            , Compute Mul regA regB regC
            , Push regC
        ]

compileExpr (ExprBrac a) lut = (compileExpr a lut)

compileExpr (ExprBool a ord b) lut = (compileExpr a lut) ++ (compileExpr b lut) ++
        [
            Pop regA
            , Pop regB
            , Compute cmpOp regA regB regC
            , Push regC
        ]
        where cmpOp = getCmpOp ord

compileExpr (ExprBin a bin b) lut = (compileExpr a lut) ++ (compileExpr b lut) ++
        [
            Pop regA
            , Pop regB
            , Compute binOp regA regB regC
            , Push regC
        ]
        where binOp = getBinOp bin

loadParam 0 = []
loadParam n arp =
        [
            Pop regA
            , Load (ImmValue arp) regB -- change this
            , Compute Sub regB regF regB
            , Store regA (DirAddr regB)
            , Compute Dec regF regF regF
        ] ++ loadParam (n-1)

-- getStatementFromLut :: String -> [[(String, Integer, Statement)]] -> Statement
-- getStatementFromLut id lut = []


-- compileExpr (ExprCall id exprs) lut = map (\x -> compileExpr x lut) exprs ++ [Load (ImmValue len) regF] ++ loadParam len newarp
--         ++
--         [
--             -- return value
--             -- return address
--             -- store own arp
--         ] ++ (compileStat ss lut newarp)
--         where len = length exprs
--               newarp = 4 + len + calculateLocalDataSize lut --notfinalized
--               (SmtDef (FunctionDef t s ps ss)) = getStatementFromLut id lut

getFuncIndex ([]:[]) = 0
getFuncIndex ([]:xss) = getFuncIndex xss
getFuncIndex (((_,_,_,n):xs):xss) = n

helpCalcLocalData [] = 0
helpCalcLocalData ([]:xss) = helpCalcLocalData xss
helpCalcLocalData (((_,_,(SmtDef (VariableDef _ _ _)),_):xs):xss) = 1 + helpCalcLocalData (xs:xss)
helpCalcLocalData (((_,_,_,_):xs):xss) = 0 + helpCalcLocalData (xs:xss)

calcLocalDataSize n lut = helpCalcLocalData b
            where (_,b) = splitAt (n+1) lut

generateLutSt :: Statement -> [[(String, Integer, Statement, Integer)]] -> [[(String, Integer, Statement, Integer)]]
generateLutSt s@(SmtDef (VariableDef _ a e)) lut = generateLutEx e newlut
            where n = (getFuncIndex (reverse lut))
                  offset = (calcLocalDataSize n lut) + 1 -- points to the caller's arp
                  newlut = (init lut) ++ [((last lut)) ++ [(a,(offset+1),s,n)]]
generateLutSt s@(SmtDef (FunctionDef _ a _ _)) lut = (init lut) ++ [((last lut)) ++ [(a,0,s,n)]]
            where n = (getFuncIndex (reverse lut))


generateLutStIf s lut = lut ++ [[]]
            where

generateLutSt _ lut = lut


generateLutEx (ExprCall s _) lut = lut ++ [[("#", -1, SmtDef (VariableDef Integer "#" (ExprConst -1)),n)]]
            where n = (length lut)



compileStat s@(SmtDef (VariableDef _ _ e)) lut arp = (compileExpr e lut) ++
            [
                Pop regA
                , Store regA (DirAddr memad)
            ]
            where memad = arp + offset + 1 -- points to the caller's arp
                  (_,offset,_,_)= (last (last lut))

compileStat s@(SmtDef (FunctionDef _ _ _ _)) lut arp = []
compileStat s@(SmtIf e strue sfalse) lut arp = (compileExpr e lut) ++
            [
                Pop regA -- regA result might be 0 or 1
                , Branch regA (Rel (lenfalse+1))
            ] ++ insfalse ++ instrue
            where newlut = (generateLutStIf s lut) -- evaluate later on
                  instrue = compileListStat strue lut arp
                  lentrue = (length instrue)
                  insfalse = compileListStat sfalse lut arp
                  lenfalse = (length insfalse)

compileStat s@(SmtWhile e sloop) lut arp = (compileExpr e lut) ++
            [
                Pop regA
                , Branch regA (Rel (lenloop + 1))
            ] ++ insloop
            where newlut = (generateLutSt s lut) --evaluate later on
                  insloop = compileListStat sloop lut arp ++ [ Jump (Rel -(lenloop + 1))]
                  lenloop = (length insloop)



-- lut must be an empty array, program ss is a result of parsing, and arp is 0
compileProg :: Program -> [[(String, Integer, Statement, Integer)]] -> Int -> [Instruction]
compileProg (Program ss) lut arp = (compileListStat ss lut arp)

compileListStat [] lut arp = []
compileListStat (s:ss) lut arp = (compileStat s newlut arp) ++ (compileListStat ss newlut arp)
            where newlut = (generateLutSt s lut)

-- test CodeGen

getExpr str = parse parseExpression [] str
unEither (Right e) = e

testCodeGenExpr = compileExpr (unEither (getExpr "")) []