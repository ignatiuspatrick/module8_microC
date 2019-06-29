module CodeGen where

import Sprockell
import LookUpTable
import Frontend
import Grammar
import Text.ParserCombinators.Parsec
import Debug.Trace


-- Instruction generators

------------- COMPILE PROGRAM
-- lut must be an empty array, program ss is a result of parsing, and arp is 0
compileProg :: Program -> [[(String, Integer, Statement, Integer)]] -> Int -> [[Instruction]]
compileProg (Program ss) lut arp = [res] where res = [ Load (ImmValue (fromIntegral arp)) regF ] ++ (compileListStat ss lut arp) ++ [ WriteInstr regA numberIO , EndProg ]



------------- COMPILE LIST OF STATEMENTS
compileListStat [] lut arp = []
compileListStat (s:ss) lut arp = (compileStat s newlut arp) ++ (compileListStat ss newlut arp)
            where newlut = (generateLutSt s lut)






------------- COMPILE STATEMENT
compileStat :: Statement -> [[(String, Integer, Statement, Integer)]] -> Int -> [Instruction]
compileStat s@(SmtDef (VariableDef _ _ e)) lut arp = (compileExpr arp e lut) ++
            [
                Pop regA
                , Store regA (DirAddr memad)
            ]
            where memad = fromIntegral ((fromIntegral arp) + offset) -- points to the caller's arp
                  (_,offset,_,_) = (last (last lut))

compileStat s@(SmtDef (FunctionDef _ _ _ _)) lut arp = []
compileStat s@(SmtIf e strue sfalse) lut arp = (compileExpr arp e lut) ++
            [
                Pop regA -- regA result might be 0 or 1
                , Branch regA (Rel (lenfalse+1))
            ] ++ insfalse ++ instrue
            where newlut = (generateLutSt s lut) -- evaluate later on
                  instrue = compileListStat strue newlut arp
                  lentrue = (length instrue)
                  insfalse = compileListStat sfalse newlut arp
                  lenfalse = (length insfalse)

compileStat s@(SmtWhile e sloop) lut arp = (compileExpr arp e lut) ++
            [
                Pop regA
                , Branch regA (Rel (lenloop + 1))
            ] ++ insloop
            where newlut = (generateLutSt s lut) --evaluate later on arp
                  insloop = compileListStat sloop newlut arp ++ [ Jump (Rel (negate (lenloop + 1)))]
                  lenloop = (length insloop)

compileStat s@(SmtFork es s1 s2) lut arp = []


compileStat s@(SmtRet e) lut arp = (compileExpr arp e lut) ++
            [
                Pop regA
                , Store regA (DirAddr addr)
            ] where addr = (fromIntegral arp - 2)

compileStat s@(SmtAss id e) lut arp = (compileExpr arp e newlut) ++
            [
                    Pop regA
                    , Store regA (DirAddr addr)
            ]
            where newlut = generateLutEx e lut
                  addr = fromIntegral (getOffsetById id (reverse newlut))

compileStat s@(SmtCall id exprs) lut arp =
                (concat (map (\x -> compileExpr arp x lut) exprs)) ++     -- compile arguments
                [
                    Load (ImmValue newarp) regF                         -- load new arp into regF
                    , Store regF (ImmValue arp)                       -- store caller arp in correct place
                ] ++
                loadParam len newarp ++                                  -- load in params into their field
                (compileListStat ss lut newarp) ++ [                     -- generate code for the function
                    Load (ImmValue arp) regF                            -- restore arp
                ]
                where len = toInteger (length exprs)
                      n = getFuncIndex lut
                      newarp = arp + fromIntegral (3 + len + calcLocalDataSize n lut)
                      retVal = newarp - 2
                      (SmtDef (FunctionDef t s ps ss)) = getStatementFromLut (fromIntegral n) id lut


compileStat s@(SmtLock id) lut arp = []
compileStat s@(SmtUnlock id) lut arp = []







------------- COMPILE EXPRESSION

compileExpr :: Int -> Expression -> [[(String, Integer, Statement, Integer)]] -> [Instruction]
compileExpr arp (ExprConst a) lut =
        [
                Load (ImmValue (fromIntegral a)) regA -- load a into register 0
                , Push regA -- push into register A
        ]

-- for true put 1, and for false put 0
compileExpr arp (ExprTrue _) lut =
        [
                Load (ImmValue (intBool True)) regA
                , Push regA
        ]

compileExpr arp (ExprFalse _) lut =
        [
                Load (ImmValue (intBool False)) regA
                , Push regA
        ]

-- find id in lut, get offset, get arp, add offset to arp, load result -> regA
compileExpr arp (ExprVar id) lut =
        getPathToAR id lut ++
        [
            Load (ImmValue offset) regC
            , Compute Add regC regE regE
            , Load (IndAddr regE) regA -- change a later with the offset
            , Push regA
        ]
        where offset = (fromIntegral (getOffsetById id (reverse lut)))


compileExpr arp(ExprAdd a b) lut = (compileExpr arp a lut) ++ (compileExpr arp b lut) ++
        [
            Pop regA
            , Pop regB
            , Compute Add regA regB regC
            , Push regC
        ]

compileExpr arp (ExprSubtract a b) lut = (compileExpr arp a lut) ++ (compileExpr arp b lut) ++
        [
            Pop regA
            , Pop regB
            , Compute Sub regA regB regC
            , Push regC
        ]

compileExpr arp (ExprMult a b) lut = (compileExpr arp a lut) ++ (compileExpr arp b lut) ++
        [
            Pop regA
            , Pop regB
            , Compute Mul regA regB regC
            , Push regC
        ]

compileExpr arp (ExprBrac a) lut = (compileExpr arp a lut)

compileExpr arp (ExprBool a ord b) lut = (compileExpr arp a lut) ++ (compileExpr arp b lut) ++
        [
            Pop regA
            , Pop regB
            , Compute cmpOp regA regB regC
            , Push regC
        ]
        where cmpOp = getCmpOp ord

compileExpr arp (ExprBin a bin b) lut = (compileExpr arp a lut) ++ (compileExpr arp b lut) ++
        [
            Pop regA
            , Pop regB
            , Compute binOp regA regB regC
            , Push regC
        ]
        where binOp = getBinOp bin



compileExpr arp e@(ExprCall id exprs) lut  =
        (concat (map (\x -> compileExpr arp x lut) exprs)) ++     -- compile arguments
        [
            Load (ImmValue newarp) regF                         -- load new arp into regF
            , Store regF (ImmValue arp)                       -- store caller arp in correct place
        ] ++
        loadParam len newarp ++                                  -- load in params into their field
        (compileListStat ss newlut newarp) ++ [                     -- generate code for the function
            Load (DirAddr retVal) regA                         -- get ret value
            , Push regA
            , Load (ImmValue arp) regF                            -- restore arp
        ]
        where len = toInteger (length exprs)
              newlut = generateLutEx e lut
              n = getFuncIndex lut
              newarp = arp + fromIntegral (3 + len + calcLocalDataSize n lut)
              retVal = newarp - 2
              (SmtDef (FunctionDef t s ps ss)) = getStatementFromLut (fromIntegral n) id lut





------------- HELPERS

loadParam :: Integer -> Int -> [Instruction]
loadParam 0 arp = []
loadParam n arp =
        [
            Pop regA
            , Load (ImmValue (2 + (fromIntegral n))) regB
            , Compute Sub regF regB regB
            , Store regA (IndAddr regB)
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


getPathToAR id lut =
                [Compute Add regF reg0 regE] ++ -- get current arp into regE
                (map (\x -> (Load (IndAddr regE) regE) ) (init [0..n])) -- go to parent's AR n times
            where n = getChangeInN id (reverse lut) (getFuncIndex (reverse lut))


getChangeInN :: String -> [[(String, Integer, Statement, Integer)]] -> Integer -> Int
getChangeInN id ([]:xss) n = getChangeInN id (xss) n
getChangeInN id (((a,b,c,d):xs):xss) currentN
    | a == id = 0
    | d /= currentN = 1 + (getChangeInN id (((a,b,c,d):xs):xss) d)
    | otherwise = getChangeInN id (xs:xss) currentN


------------- TESTING

getExpr str = parse parseExpression [] str
unEither (Right e) = e

testCodeGenExpr = compileExpr 0 (unEither (getExpr "")) []