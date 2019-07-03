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
compileProg :: Program -> [[(String, Integer, Statement, Integer)]] -> Int -> [(String, Int)] -> [[Instruction]]
compileProg (Program ss) lut arp shared =
            map (++ [ WriteInstr regA numberIO , EndProg ]) res
                where res = (compileListStat ss lut arp shared [[ Load (ImmValue (fromIntegral arp)) regF ]] 0)



------------- COMPILE LIST OF STATEMENTS
compileListStat :: [Statement] -> [[(String, Integer, Statement, Integer)]] -> Int -> [(String, Int)] -> [[Instruction]] -> Int -> [[Instruction]]
compileListStat [] lut arp shared instr threadNo = instr
compileListStat (s:ss) lut arp shared instr threadNo =
        (compileListStat ss newlut arp shared newInstr threadNo)
            where newlut = (generateLutSt s lut)
                  newInstr = (compileStat s newlut arp shared instr threadNo)






------------- COMPILE STATEMENT
compileStat :: Statement -> [[(String, Integer, Statement, Integer)]] -> Int -> [(String, Int)] -> [[Instruction]] -> Int -> [[Instruction]]
compileStat s@(SmtDef (VariableDef _ _ e)) lut arp shared instr threadNo = appendToList res threadNo
             [
                Pop regA
                , Store regA (DirAddr memad)
             ]
            where res = (compileExpr arp e lut shared  instr threadNo)
                  memad = fromIntegral ((fromIntegral arp) + offset) -- points to the caller's arp
                  (_,offset,_,_) = (last (last lut))

compileStat s@(SmtDef (FunctionDef _ _ _ _)) lut arp shared instr threadNo = instr
compileStat s@(SmtIf e strue sfalse) lut arp shared instr threadNo = instrue
            where newlut = (generateLutSt s lut) -- evaluate later on
                  iE = (compileExpr arp e lut shared instr threadNo)
                  exprInstr = appendToList iE threadNo
                        [
                           Pop regA -- regA result might be 0 or 1
                           , Branch regA (Rel (lenfalse+1))
                        ]
                  instrue = compileListStat strue newlut arp shared insfalse threadNo
                  lentrue = (length (instrue !! threadNo)) - (length (insfalse !! threadNo))
                  insfalse = appendToList (compileListStat sfalse newlut arp shared exprInstr threadNo) threadNo [ Jump (Rel (lentrue+1)) ]
                  lenfalse = (length (insfalse !! threadNo)) - (length (exprInstr !! threadNo))

compileStat s@(SmtWhile e sloop) lut arp shared instr threadNo = insloop
            where newlut = (generateLutSt s lut) --evaluate later on arp
                  insloop = appendToList (compileListStat sloop newlut arp shared exprInstr threadNo) threadNo [ Jump (Rel (negate (lenloop + lencond - 1)))]
                  lenloop = (length (insloop !! threadNo)) - (length (exprInstr !! threadNo))
                  lencond = (length (exprInstr !! threadNo)) - (length (instr !! threadNo))
                  exprInstr = appendToList (compileExpr arp e lut shared instr threadNo) threadNo
                                          [
                                              Pop regA
                                              , Branch regA (Rel 2)
                                              , Jump (Rel (lenloop + 1))
                                          ]



compileStat s@(SmtRet e) lut arp shared instr threadNo = appendToList (compileExpr arp e lut shared  instr threadNo) threadNo
            [
                Pop regA
                , Store regA (DirAddr addr)
            ] where addr = (fromIntegral arp - 2)

compileStat s@(SmtAss id e) lut arp shared instr threadNo =
            appendToList (compileExpr arp e newlut shared instr threadNo) threadNo
                (if inShared id shared then getFromShared else getFromLocal)
            where newlut = generateLutEx e lut
                  offset = fromIntegral (getOffsetById id (reverse newlut))
                  getFromLocal = (getPathToAR id lut ++
                                             [
                                                 Load (ImmValue offset) regC
                                                 , Compute Add regC regE regE
                                                 , Pop regA
                                                 , Store regA (IndAddr regE)
                                             ])
                  getFromShared = [
                                    Pop regC,
                                    ReadInstr (DirAddr lockAddr),
                                    Receive regA,
                                    Branch regA (Rel 2),
                                    Jump (Rel 6),
                                    ReadInstr (DirAddr ownerAddr),
                                    Receive regA,
                                    Compute Equal regA regSprID regA,
                                    Branch regA (Rel 2),
                                    Jump (Rel (negate 8)),
                                    WriteInstr regC (DirAddr addr)
                                  ]
                  addr = getSharedAddr id shared
                  lockAddr = addr + 1
                  ownerAddr = addr + 2

compileStat s@(SmtCall id exprs) lut arp shared instr threadNo =
                appendToList (compileListStat ss lut newarp shared exprIntrs threadNo) threadNo   -- generate code for the function
                [
                    Load (ImmValue arp) regF                            -- restore arp
                ]
                where len = toInteger (length exprs)
                      n = getFuncIndex lut
                      newarp = arp + fromIntegral (3 + len + calcLocalDataSize n lut)
                      retVal = newarp - 2
                      (SmtDef (FunctionDef t s ps ss)) = getStatementFromLut (fromIntegral n) id lut
                      exprIntrs = appendToList (compileListExprs arp exprs lut shared instr threadNo) threadNo     -- compile arguments
                                          ([
                                              Load (ImmValue newarp) regF                         -- load new arp into regF
                                              , Store regF (ImmValue arp)                       -- store caller arp in correct place
                                          ] ++
                                          loadParam len newarp)



compileStat s@(SmtFork exprs s1 s2) lut arp shared instr threadNo =
        appendToList second threadNo (moveFromSharedToLocal exprs lut arp newShared)
            where newShared = movetToShared s lut arp shared t1
                  t1 = length instr
                  t2 = (length instr) + 1
                  moved = trace("shared " ++ (show newShared)) $ appendToList instr threadNo (moveToSharedMemory exprs lut arp newShared)
                  mainWithRelease = appendToList moved threadNo [WriteInstr reg0 (DirAddr commAddr1), WriteInstr reg0 (DirAddr commAddr2)]
                  mainWithWait = appendToList mainWithRelease threadNo [
                                                                ReadInstr (DirAddr commAddr1),
                                                                Receive regA,
                                                                Load (ImmValue (negate 1)) regC,
                                                                Compute Equal regA regC regA,
                                                                Branch regA (Rel 2),
                                                                Jump (Rel (negate 5)),

                                                                ReadInstr (DirAddr commAddr2),
                                                                Receive regA,
                                                                Load (ImmValue (negate 1)) regC,
                                                                Compute Equal regA regC regA,
                                                                Branch regA (Rel 2),
                                                                Jump (Rel (negate 5))
                                                             ]

                  prepped = (mainWithWait ++ waitForBarrier1 ++ waitForBarrier2)
                  waitForBarrier1 = [[ TestAndSet (DirAddr commAddr1), Receive regA, Branch regA (Rel 2), Jump (Rel (negate 3)) ]]
                  waitForBarrier2 = [[ TestAndSet (DirAddr commAddr2), Receive regA, Branch regA (Rel 2), Jump (Rel (negate 3)) ]]

                  first = appendToList (compileListStat s1 lut arp newShared prepped t1) t1 [ Load (ImmValue (negate 1)) regC, WriteInstr regC (DirAddr commAddr1) ]

                  second = appendToList (compileListStat s2 lut arp newShared first t2) t2 [ Load (ImmValue (negate 1)) regC, WriteInstr regC (DirAddr commAddr2) ]

                  (commAddr1, commAddr2) = getCommAddr newShared t1


compileStat s@(SmtLock id) lut arp shared instr threadNo =
        appendToList instr threadNo
        [
            TestAndSet (DirAddr lockAddr)
          , Receive regA
          , Branch regA (Rel 2)
          , Jump (Rel (negate 3))
          , WriteInstr regSprID (DirAddr ownerAddr)
        ]
            where addr = findInShared id shared
                  lockAddr = addr + 1
                  ownerAddr = addr + 2

compileStat s@(SmtUnlock id) lut arp shared instr threadNo =
        appendToList instr threadNo
        [
            WriteInstr reg0 (DirAddr lockAddr),
            Load (ImmValue (negate 1)) regA,
            WriteInstr regA (DirAddr ownerAddr)
        ]
            where addr = findInShared id shared
                  lockAddr = addr + 1
                  ownerAddr = addr + 2


moveToSharedMemory :: [Expression] -> [[(String, Integer, Statement, Integer)]] -> Int -> [(String, Int)] -> [Instruction]
moveToSharedMemory exprs lut arp shared =
            concat (map (\x -> getVarToMem x lut shared) ids)
                    where ids = getIdsFromVars exprs

moveFromSharedToLocal :: [Expression] -> [[(String, Integer, Statement, Integer)]] -> Int -> [(String, Int)] -> [Instruction]
moveFromSharedToLocal exprs lut arp shared =
            concat (map (\x -> getVarToLocal x lut shared) ids)
                    where ids = getIdsFromVars exprs


getVarToLocal id lut shared =
        (getPathToAR id lut ++
        [
            Load (ImmValue offset) regC
            , Compute Add regC regE regE
            , ReadInstr (DirAddr addr)
            , Receive regA
            , Store regA (IndAddr regE) -- store at calculated dir
        ])
        where addr = findInShared id shared
              offset = (fromIntegral (getOffsetById id (reverse lut)))


getVarToMem id lut shared =
        (getPathToAR id lut ++
        [
            Load (ImmValue offset) regC
            , Compute Add regC regE regE
            , Load (IndAddr regE) regA
            , WriteInstr regA (DirAddr addr)
            , WriteInstr reg0 (DirAddr (addr+1))
            , WriteInstr reg0 (DirAddr (addr+2))
        ])
        where addr = findInShared id shared
              offset = (fromIntegral (getOffsetById id (reverse lut)))


findInShared id ((x, addr):shared) = if x == id then addr else findInShared id shared







------------- COMPILE LIST EXPRESSION

compileListExprs :: Int -> [Expression] -> [[(String, Integer, Statement, Integer)]] -> [(String, Int)] -> [[Instruction]] -> Int -> [[Instruction]]
compileListExprs arp [] lut shared instr threadNo = instr
compileListExprs arp (e:exprs) lut shared instr threadNo = res
            where res = compileListExprs arp (exprs) lut shared (compileExpr arp e lut shared instr threadNo) threadNo




------------- COMPILE EXPRESSION

compileExpr :: Int -> Expression -> [[(String, Integer, Statement, Integer)]] -> [(String, Int)] -> [[Instruction]] -> Int -> [[Instruction]]
compileExpr arp (ExprConst a) lut shared instr threadNo =
        appendToList instr threadNo
        [
                Load (ImmValue (fromIntegral a)) regA -- load a into register 0
                , Push regA -- push into register A
        ]

-- for true put 1, and for false put 0
compileExpr arp (ExprTrue _) lut shared instr threadNo =
        appendToList instr threadNo
        [
                Load (ImmValue (intBool True)) regA
                , Push regA
        ]

compileExpr arp (ExprFalse _) lut shared instr threadNo =
        appendToList instr threadNo
        [
                Load (ImmValue (intBool False)) regA
                , Push regA
        ]

-- find id in lut, get offset, get arp, add offset to arp, load result -> regA
compileExpr arp (ExprVar id) lut shared instr threadNo =
        appendToList instr threadNo (if inShared id shared then getFromShared else getFromLocal)
        where offset = (fromIntegral (getOffsetById id (reverse lut)))
              getFromLocal = (getPathToAR id lut ++
                                   [
                                       Load (ImmValue offset) regC
                                       , Compute Add regC regE regE
                                       , Load (IndAddr regE) regA -- change a later with the offset
                                       , Push regA
                                   ])
              getFromShared = [
                                ReadInstr (DirAddr lockAddr),
                                Receive regA,
                                Branch regA (Rel 2),
                                Jump (Rel 6),
                                ReadInstr (DirAddr ownerAddr),
                                Receive regA,
                                Compute Equal regA regSprID regA,
                                Branch regA (Rel 2),
                                Jump (Rel (negate 8)),
                                ReadInstr (DirAddr addr),
                                Receive regA,
                                Push regA
                              ]
              addr = getSharedAddr id shared
              lockAddr = addr + 1
              ownerAddr = addr + 2


compileExpr arp(ExprAdd a b) lut shared instr threadNo =
        appendToList add2 threadNo
        [
            Pop regA
            , Pop regB
            , Compute Add regB regA regC
            , Push regC
        ]
        where add1 = compileExpr arp a lut shared instr threadNo
              add2 = compileExpr arp b lut shared add1 threadNo

compileExpr arp (ExprSubtract a b) lut shared instr threadNo =
        appendToList sub2 threadNo
        [
            Pop regA
            , Pop regB
            , Compute Sub regB regA regC
            , Push regC
        ]
        where sub1 = compileExpr arp a lut shared instr threadNo
              sub2 = compileExpr arp b lut shared sub1 threadNo

compileExpr arp (ExprMult a b) lut shared instr threadNo =
        appendToList mult2 threadNo
        [
            Pop regA
            , Pop regB
            , Compute Mul regB regA regC
            , Push regC
        ]
        where mult1 = compileExpr arp a lut shared instr threadNo
              mult2 = compileExpr arp b lut shared mult1 threadNo

compileExpr arp (ExprBrac a) lut shared instr threadNo = (compileExpr arp a lut shared instr threadNo)

compileExpr arp (ExprBool a ord b) lut shared  instr threadNo =
        appendToList bool2 threadNo
        [
            Pop regA
            , Pop regB
            , Compute cmpOp regB regA regC
            , Push regC
        ]
        where cmpOp = getCmpOp ord
              bool1 = compileExpr arp a lut shared instr threadNo
              bool2 = compileExpr arp b lut shared bool1 threadNo

compileExpr arp (ExprBin a bin b) lut shared  instr threadNo =
        appendToList bin2 threadNo
        [
            Pop regA
            , Pop regB
            , Compute binOp regB regA regC
            , Push regC
        ]
        where binOp = getBinOp bin
              bin1 = compileExpr arp a lut shared instr threadNo
              bin2 = compileExpr arp b lut shared bin1 threadNo



compileExpr arp e@(ExprCall id exprs) lut shared instr threadNo =
        appendToList stats threadNo [ Load (DirAddr retVal) regA, Push regA , Load (ImmValue arp) regF ]
        where len = toInteger (length exprs)
              newlut = generateLutEx e lut
              n = getFuncIndex lut
              newarp = arp + fromIntegral (3 + len + calcLocalDataSize n lut)
              retVal = newarp - 2
              (SmtDef (FunctionDef t s ps ss)) = getStatementFromLut (fromIntegral n) id lut
              exprInstr = appendToList (compileListExprs arp exprs lut shared instr threadNo) threadNo      -- compile arguments
                      ([
                          Load (ImmValue newarp) regF                         -- load new arp into regF
                          , Store regF (ImmValue arp)                       -- store caller arp in correct place
                      ] ++
                      loadParam len newarp)                                  -- load in params into their field
              stats = (compileListStat ss newlut newarp shared exprInstr threadNo)



------------- HELPERS

inShared id [] = False
inShared id ((s, _):shared) = if id == s then True else inShared id shared
getSharedAddr id ((s, addr):shared) = if id == s then addr else getSharedAddr id shared

loadParam :: Integer -> Int -> [Instruction]
loadParam 0 arp = []
loadParam n arp =
        [
            Pop regA
            , Load (ImmValue (2 + (fromIntegral n))) regB
            , Compute Sub regF regB regB
            , Store regA (IndAddr regB)
        ] ++ loadParam (n-1) arp


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
    | d /= currentN = 1 + (getChangeInN id (((a,b,c,d):xs):xss) d)
    | a == id = 0
    | otherwise = getChangeInN id (xs:xss) currentN


appendToList :: [[Instruction]] -> Int -> [Instruction] -> [[Instruction]]
appendToList (xs:xss) 0 list = (xs ++ list) : xss
appendToList s@(xs:xss) n list = xs : (appendToList xss (n-1) list)


getCommAddr shared threadNo = (res, res + 1)
            where res = ((length shared) * spaceInSharedMemForVar) + threadNo

------------- TESTING

getExpr str = parse parseExpression [] str
unEither (Right e) = e

testCodeGenExpr = compileExpr 0 (unEither (getExpr "")) []