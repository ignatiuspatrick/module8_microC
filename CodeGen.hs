module CodeGen where

import Sprockell
import LookUpTable
import Frontend
import Grammar
import Text.ParserCombinators.Parsec



-- Instruction generators

------------- COMPILE PROGRAM
-- lut must be an empty array, program ss is a result of parsing, and arp is 0
compileProg :: Program -> [[(String, Integer, Statement, Integer)]] -> Int -> [Instruction]
compileProg (Program ss) lut arp = [ Load (ImmValue (fromIntegral arp)) regF ] ++ (compileListStat ss lut arp)



------------- COMPILE LIST OF STATEMENTS
compileListStat [] lut arp = []
compileListStat (s:ss) lut arp = (compileStat s newlut arp) ++ (compileListStat ss newlut arp)
            where newlut = (generateLutSt s lut)






------------- COMPILE STATEMENT
compileStat :: Statement -> [[(String, Integer, Statement, Integer)]] -> Int -> [Instruction]
compileStat s@(SmtDef (VariableDef _ _ e)) lut arp = (compileExpr e lut) ++
            [
                Pop regA
                , Store regA (DirAddr memad)
            ]
            where memad = fromIntegral ((fromIntegral arp) + offset + 1) -- points to the caller's arp
                  (_,offset,_,_)= (last (last lut))

compileStat s@(SmtDef (FunctionDef _ _ _ _)) lut arp = []
compileStat s@(SmtIf e strue sfalse) lut arp = (compileExpr e lut) ++
            [
                Pop regA -- regA result might be 0 or 1
                , Branch regA (Rel (lenfalse+1))
            ] ++ insfalse ++ instrue
            where newlut = (generateLutSt s lut) -- evaluate later on
                  instrue = compileListStat strue newlut arp
                  lentrue = (length instrue)
                  insfalse = compileListStat sfalse newlut arp
                  lenfalse = (length insfalse)

compileStat s@(SmtWhile e sloop) lut arp = (compileExpr e lut) ++
            [
                Pop regA
                , Branch regA (Rel (lenloop + 1))
            ] ++ insloop
            where newlut = (generateLutSt s lut) --evaluate later on arp
                  insloop = compileListStat sloop newlut arp ++ [ Jump (Rel (negate (lenloop + 1)))]
                  lenloop = (length insloop)

compileStat s@(SmtFork es s1 s2) lut arp = []


compileStat s@(SmtRet e) lut arp = (compileExpr e lut) ++
                [
                    -- pop res from stack, put in return value
                    -- get return address
                    -- jump back to caller
                ]
compileStat s@(SmtAss id e) lut arp = []
compileStat s@(SmtCall id es) lut arp = []
compileStat s@(SmtLock id) lut arp = []
compileStat s@(SmtUnlock id) lut arp = []







------------- COMPILE EXPRESSION

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



compileExpr (ExprCall id exprs) lut =
        (concat (map (\x -> compileExpr x lut) exprs)) ++  -- compile arguments
        [Load (ImmValue newarp) regF] ++      -- load new arp into regF
        loadParam len newarp ++               -- load in params into their field
        [
            -- return value
            -- store own arp
        ] ++ (compileListStat ss lut newarp) ++ [
            -- push return value on stack
            -- restore arp
        ]
        where len = toInteger (length exprs)
              n = getFuncIndex lut
              newarp = fromIntegral (4 + len + calcLocalDataSize n lut)
              (SmtDef (FunctionDef t s ps ss)) = getStatementFromLut (fromIntegral n) id lut








------------- HELPERS

loadParam :: Integer -> Int -> [Instruction]
loadParam 0 arp = []
loadParam n arp =
        [
            Pop regA
            , Load (ImmValue arp) regB -- change this
            , Compute Sub regB regF regB
            , Store regA (DirAddr regB)
            , Compute Decr regF regF regF
        ] ++ loadParam (n-1) arp


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
getOffsetById id (((a,b,c,d):xs):xss)
    | a == id = b
    | otherwise = getOffsetById id (xs:xss)








------------- TESTING

getExpr str = parse parseExpression [] str
unEither (Right e) = e

testCodeGenExpr = compileExpr (unEither (getExpr "")) []