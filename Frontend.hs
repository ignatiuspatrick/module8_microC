module Frontend where

import Data.List
import Data.Char

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Debug.Trace
import Grammar


-- Programs
parseProgram = Program <$> (many parseStatement)
parseDefinition =
               (FunctionDef <$> (reserved "function" *> parseType) <*> identifier <*> parens (commaSep parseParam) <*> braces (many parseStatement))
               <|> (VariableDef <$> parseType <*> (identifier <* symbol "=") <*> (parseExpression <* semi))

-- Variables
parseType = (IntType <$> reserved "int") <|> (BoolType <$> reserved "boolean") <|> (VoidType <$> reserved "void")

-- Functions
parseParam = Param <$> parseType <*> identifier

-- Language
parseStatement =
               SmtIf <$> (reserved "if" *> (parens parseExpression)) <*> (braces (many parseStatement)) <*> option [] (reserved "else" *> (braces (many parseStatement)))
              <|> SmtWhile <$> (reserved "while" *> (parens parseExpression)) <*> (braces (many parseStatement))
              <|> SmtRet <$> (reserved "return" *> parseExpression <* semi)
              <|> SmtFork <$> (reserved "fork" *> (parens (commaSep identifier))) <*> (braces (many parseStatement)) <*> (braces (many parseStatement))
              <|> SmtLock <$> (reserved "lock" *> identifier <* semi)
              <|> SmtUnlock <$> (reserved "unlock" *> identifier <* semi)
              <|> try (SmtDef <$> parseDefinition)
              <|> try (SmtAss <$> (identifier <* symbol "=") <*> parseExpression <* semi)
              <|> SmtCall <$> identifier <*> (parens (commaSep parseExpression)) <* semi



-- Algebra

add' = (\_ -> (ExprAdd)) <$> reserved "+"

sub' = (\_ -> (ExprSubtract)) <$> reserved "-"

mult' = (\_ -> (ExprMult)) <$> reserved "*"

parseExpression =
                (try (ExprBin <$> parseBoolean <*> parseBinary <*> parseExpression))
                <|> parseBoolean

parseBoolean = try (ExprBool <$> parseArithmetic1 <*> parseOrder <*> parseBoolean)
            <|> parseArithmetic1

parseArithmetic1 = (parseArithmetic2 `chainr1` add')
parseArithmetic2 = (parseArithmetic3 `chainr1` sub')
parseArithmetic3 = (parseFact `chainr1` mult')


parseFact =
            (ExprConst <$> integer)
            <|> (ExprTrue <$> reserved "true")
            <|> (ExprFalse <$> reserved "false")
            <|> try (ExprCall <$> identifier <*> (parens (commaSep parseExpression)))
            <|> (ExprVar <$> identifier)
            <|> (ExprBrac <$> (parens parseExpression))

parseOrder =
          OrderLE <$> reserved "<="
          <|> OrderLT <$> reserved "<"
          <|> OrderEQ <$> reserved "=="
          <|> OrderNE <$> reserved "!="
          <|> OrderGE <$> reserved ">="
          <|> OrderGT <$> reserved ">"

parseBinary = BinaryAnd <$> reserved "&&"
            <|> BinaryOr <$> reserved "||"


----------------- TYPE CHECKING


initProg :: Program -> [[(String, Statement)]] -> [[(String, Statement)]]
initProg (Program ([])) scopes = scopes
initProg (Program (x:xs)) scopes = initProg (Program xs) (initStatement x scopes)

initStatement :: Statement -> [[(String, Statement)]] -> [[(String, Statement)]]
initStatement stm@(SmtDef (VariableDef a id expr)) scopes =
                if def == Left defNotFound
                then (if checkExpr expr scopes (strFromType a)
                    then (init scopes) ++ [((last scopes) ++ [(id, stm)])]
                    else error("Type error in variable definition! The variable '" ++ id ++ "' has a wrong type."))
                else error ("Type error in variable definition! The identifier '" ++ id ++ "' has already been used.")
                where def = getTopLevelDefinition id scopes

initStatement stm@(SmtDef (FunctionDef a id params statements)) scopes =
        if def == (Left (defNotFound))
            then
                if (a == VoidType ()) || (isReturnSmt (last statements))
                then res
                else error ("Type error in function definition! The function '" ++ id ++ "' has no return statement.")
            else error ("Type error in function definition! The identifier '" ++ id ++ "' has already been used.")
                where def = getTopLevelDefinition id scopes
                      updated = ((init scopes) ++ [((last scopes) ++ [(id, stm)])]) ++ [[(id, stm)]]
                      func (Param t i) = if t == IntType ()                                               -- fake a definition
                                              then (i, (SmtDef (VariableDef t id (ExprConst 0))))
                                              else (i, (SmtDef (VariableDef t id (ExprTrue ()))))
                      res = initScope statements ((init updated) ++ [((last updated) ++ (map func params))])

initStatement stm@(SmtIf e stm1 stm2) scopes =
            if checkExpr e scopes "boolean"
                then res
                else error ("Type error in if condition!" )
                where res = initScope stm2 (res1 ++ [[]])
                      res1 = (initScope stm1 (scopes ++ [[]]))

initStatement stm@(SmtWhile e stms) scopes =
            if checkExpr e scopes "boolean"
                then res
                else error ("Type error in while condition!")
                where res = initScope stms (scopes ++ [[]])

initStatement stm@(SmtFork ids s1 s2) scopes =
            if checkForkParams ids scopes
            then
                if checkForkLocks ids s1 && checkForkLocks ids s2
                then scopes ++ res1 ++ res2
                else error ("Locking a variable that wasn't moved!")
            else error ("Fork arguments have not been initialized before!")
            where res1 = initScope s1 lut
                  res2 = initScope s2 lut
                  lut = [[] ++ (map (\x -> let (Right def) = getDefinition x scopes in (x, def)) ids)]


initStatement stm@(SmtLock e) scopes = scopes
initStatement stm@(SmtUnlock e) scopes = scopes

initStatement stm@(SmtRet e) scopes = scopes

initStatement stm@(SmtAss id e) scopes =
            if def /= (Left defNotFound) && checkExpr e scopes (strFromType t)
                then scopes
                else error ("Type error in assignment statement! Variable '" ++ id ++ "' was not defined.")
                where def = getDefinition id scopes
                      t = getTypeFromDef def

initStatement stm@(SmtCall id exps) scopes =
                    if def /= Left defNotFound
                    then
                        if checkParams exps params scopes
                        then scopes
                        else error ("Type mismatch in func call statement! The parameters do not fit the definition!")
                    else error ("Type error in function call! Function '" ++ id ++ "' was not defined.")
                      where def = getDefinition id scopes
                            params = getParamsFromDef def
                            t = getTypeFromDef def


----------------- EXPRESSION CHECKING

checkExpr :: Expression -> [[(String, Statement)]] -> String -> Bool
checkExpr (ExprConst a) scopes exprType = exprType == "int"
checkExpr (ExprTrue _) scopes exprType = exprType == "boolean"
checkExpr (ExprFalse _) scopes exprType = exprType == "boolean"
checkExpr (ExprAdd e1 e2) scopes exprType = exprType == "int" && checkExpr e1 scopes "int" && checkExpr e2 scopes "int"
checkExpr (ExprSubtract e1 e2) scopes exprType = exprType == "int" && checkExpr e1 scopes "int" && checkExpr e2 scopes "int"
checkExpr (ExprMult e1 e2) scopes exprType = exprType == "int" && checkExpr e1 scopes "int" && checkExpr e2 scopes "int"
checkExpr (ExprBrac e) scopes exprType = checkExpr e scopes exprType
checkExpr (ExprBool e1 o e2) scopes exprType =
                    (checkExpr e1 scopes "int" && checkExpr e2 scopes "int")
                     ||
                    (checkExpr e1 scopes "boolean" && checkExpr e2 scopes "boolean")
checkExpr (ExprBin e1 b e2) scopes exprType =
                    (checkExpr e1 scopes "boolean" && checkExpr e2 scopes "boolean")


checkExpr (ExprCall id xs) scopes exprType =
                if def /= (Left defNotFound) then
                    checkParams xs params scopes &&
                     (if exprType == "int"
                         then t == IntType ()
                         else t == BoolType ())
                else error ("Function definition for '" ++ id ++ "' not found!")
                where def = getDefinition id scopes
                      t = getTypeFromDef def
                      params = getParamsFromDef def


checkExpr (ExprVar id) scopes exprType =
                    if def /= Left (defNotFound)
                    then
                        if exprType == "int"
                        then t == IntType ()
                        else t == BoolType ()
                    else False
                        where def = getDefinition id scopes
                              t = getTypeFromDef def



----------------- HELPERS

checkForkParams ids scopes = and (map (\x -> getDefinition x scopes /= Left defNotFound) ids)

checkForkLocks ids [] = True
checkForkLocks ids ((SmtLock id):statements) = (id `elem` ids) && checkForkLocks ids statements
checkForkLocks ids ((SmtUnlock id):statements) = (id `elem` ids) && checkForkLocks ids statements
checkForkLocks ids ((SmtIf e s1 s2):statements) = checkForkLocks ids s1 && checkForkLocks ids s2 && checkForkLocks ids statements
checkForkLocks ids ((SmtWhile e s):statements) = checkForkLocks ids s && checkForkLocks ids statements
checkForkLocks ids ((SmtFork i s1 s2):statements) = checkForkLocks (ids ++ i) s1 && checkForkLocks (ids ++ i) s2 && checkForkLocks ids statements
checkForkLocks ids ((SmtDef (FunctionDef a id params s):statements)) = checkForkLocks ids s && checkForkLocks ids statements
checkForkLocks ids (_:statements) = checkForkLocks ids statements


checkParams (e:exps) ((Param t id):params) scopes = checkExpr e scopes (if t == (IntType ()) then "int" else "boolean")
checkParams [] [] scopes = True
checkParams _ [] scopes = False
checkParams [] _ scopes = False


getDefinition :: String -> [[(String, Statement)]] -> Either String Statement
getDefinition id scopes = getDefRec id (reverse scopes)
getDefRec id [] = Left defNotFound
getDefRec id ([]:xs) = getDefRec id (xs)
getDefRec id (x:xs) = if (length filtered) > 0 then Right (filtered!!0) else getDefRec id xs
                  where filtered = [smt | (i, smt@(SmtDef _)) <- x, i == id]

getTopLevelDefinition id [] = Left defNotFound
getTopLevelDefinition id [x] = maybeToRight defNotFound (find (\(a, b) -> a == id) x)
getTopLevelDefinition id scopes = maybeToRight defNotFound found
        where found = (find (\(a, b) -> a == id) (last scopes))


-- Open scope and put in the first of the statements into it
initScope [] scopes = init scopes
initScope statements scopes = init folded
                       where folded = initializeStatements statements scopes

initializeStatements [] scopes = scopes
initializeStatements (s:statements) scopes = initializeStatements statements (initStatement s scopes)


getTypeFromDef (Right (SmtDef (VariableDef t id _))) = t
getTypeFromDef (Right (SmtDef (FunctionDef t id _ _))) = t

getParamsFromDef (Right (SmtDef (FunctionDef _ _ params _))) = params

strFromType (IntType ()) = "int"
strFromType (BoolType ()) = "boolean"

isReturnSmt (SmtRet _) = True
isReturnSmt _ = False

maybeToRight def (Just a) = Right a
maybeToRight def (Nothing) = Left def

defNotFound = "Definition not found!"


----------------- TEST

testFront p = if length (testInit (par)) >= 0 then parsed else error ("Type checking failed!")
        where par@(Right parsed) = testParser p

testInit (Right p) = initProg p [[]]

testParser p = parse parseProgram [] p

p1 = "int a = 1; function int x() { int z = 1; } int y = 0;"
p2 = "int a = 1; function int a() { int z = 1; } int y = 0;"
p3 = "int a = 1; int b = 1+1; int c = 2; function int x() { boolean a = 1; }"
p4 = "boolean x = true; if (x == true) {int x = 3;}"
p5 = "boolean x = true; if (x == 1) {int x = 3;} else {}"
p6 = "boolean x = true; if (x == true) {int x = 3; while (x > 0) { x = x - 1; }}"
p7 = "int a = 1; int b = 1+1; function boolean x() { boolean a = true; return a; } int c = x();"
p8 = "int a = 1; int b = 1+1; function boolean x() { boolean a = true; return a; } boolean c = x();"
p9 = "int a = 1; int b = 1+1; function int x(int b) { int z = 1; } int c = x(5);"
p10 = "int a = 1; int b = 1+1; function int x(int b) { int z = 1; return z + b;} int c = x(5);"
