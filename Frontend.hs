module Frontend where

import Data.List
import Data.Either
import Data.Char

import Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Debug.Trace
import Grammar
import Control.Exception


data ParseException = ParseException String
                        deriving Show
instance Exception ParseException

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
              <|> SmtFork <$> (reserved "fork" *> (parens (commaSep parseExpression))) <*> (braces (many parseStatement)) <*> (braces (many parseStatement))
              <|> SmtLock <$> (reserved "lock" *> identifier <* semi)
              <|> SmtUnlock <$> (reserved "unlock" *> identifier <* semi)
              <|> Parsec.try (SmtDef <$> parseDefinition)
              <|> Parsec.try (SmtAss <$> (identifier <* symbol "=") <*> parseExpression <* semi)
              <|> SmtCall <$> identifier <*> (parens (commaSep parseExpression)) <* semi



-- Algebra

add' = (\_ -> (ExprAdd)) <$> reserved "+"

sub' = (\_ -> (ExprSubtract)) <$> reserved "-"

mult' = (\_ -> (ExprMult)) <$> reserved "*"

parseExpression =
                (Parsec.try (ExprBin <$> parseBoolean <*> parseBinary <*> parseExpression))
                <|> parseBoolean

parseBoolean = Parsec.try (ExprBool <$> parseArithmetic1 <*> parseOrder <*> parseBoolean)
            <|> parseArithmetic1

parseArithmetic1 = (parseArithmetic2 `chainr1` add')
parseArithmetic2 = (parseArithmetic3 `chainr1` sub')
parseArithmetic3 = (parseFact `chainr1` mult')


parseFact =
            (ExprConst <$> integer)
            <|> (ExprTrue <$> reserved "true")
            <|> (ExprFalse <$> reserved "false")
            <|> Parsec.try (ExprCall <$> identifier <*> (parens (commaSep parseExpression)))
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


initProg :: Program -> Either String [[(String, Statement)]] -> Either String [[(String, Statement)]]
initProg (Program ([])) scopes = scopes
initProg (Program (x:xs)) (Right scopes) = initProg (Program xs) new
    where new = (initStatement x (Right scopes))
initProg (Program (x:xs)) (Left err) = (Left err)

initStatement :: Statement -> Either String [[(String, Statement)]] -> Either String [[(String, Statement)]]
initStatement stm@(SmtDef (VariableDef a id expr)) (Right scopes) =
                if def == Left defNotFound
                then (if checkExpr expr scopes (strFromType a)
                    then Right ((init scopes) ++ [((last scopes) ++ [(id, stm)])])
                    else Left ("Type error in variable definition! The definition of '" ++ id ++ "' is invalid."))
                else Left ("Type error in variable definition! The identifier '" ++ id ++ "' has already been used.")
                where def = getTopLevelDefinition id scopes

initStatement stm@(SmtDef (FunctionDef a id params statements)) (Right scopes) =
        if def == (Left (defNotFound))
            then
                if (a == VoidType ()) || (isReturnSmt (last statements))
                then res
                else Left ("Type error in function definition! The function '" ++ id ++ "' has no return statement.")
            else Left ("Type error in function definition! The identifier '" ++ id ++ "' has already been used.")
                where def = getTopLevelDefinition id scopes
                      updated = ((init scopes) ++ [((last scopes) ++ [(id, stm)])]) ++ [[(id, stm)]]
                      func (Param t i) = if t == IntType ()                                               -- fake a definition
                                              then (i, (SmtDef (VariableDef t id (ExprConst 0))))
                                              else (i, (SmtDef (VariableDef t id (ExprTrue ()))))
                      res = initScope statements (Right ((init updated) ++ [((last updated) ++ (map func params))]))

initStatement stm@(SmtIf e stm1 stm2) (Right scopes) =
            if checkExpr e scopes "boolean"
                then
                    if isLeft res1
                    then res1
                    else res
                else Left ("Type error in if condition!" )
                where res = initScope stm2 (Right ((fromRight [] res1) ++ [[]]))
                      res1 = (initScope stm1 (Right (scopes ++ [[]])))

initStatement stm@(SmtWhile e stms) (Right scopes) =
            if checkExpr e scopes "boolean"
                then res
                else Left ("Type error in while condition!")
                where res = initScope stms (Right (scopes ++ [[]]))

initStatement stm@(SmtFork exprs s1 s2) (Right scopes) =
            if checkForkParams exprs scopes
            then
                if checkForkLocks ids s1 && checkForkLocks ids s2
                then
                    if isLeft res1
                    then res1
                    else
                        if isLeft res2
                        then res2
                        else Right (scopes ++ (fromRight [[]] res1) ++ (fromRight [[]] res2))
                else Left ("Locking a variable that wasn't moved!")
            else Left ("Fork arguments have not been initialized before!")
            where res1 = initScope s1 (Right lut)
                  res2 = initScope s2 (Right lut)
                  lut = [[] ++ (map (\(ExprVar x) -> let (Right def) = getDefinition x scopes in (x, def)) exprs)]
                  ids = getIdsFromVars exprs


initStatement stm@(SmtLock e) scopes = scopes
initStatement stm@(SmtUnlock e) scopes = scopes

initStatement stm@(SmtRet e) scopes = scopes

initStatement stm@(SmtAss id e) (Right scopes) =
            if def /= (Left defNotFound) && checkExpr e scopes (strFromType t)
                then Right scopes
                else Left ("Type error in assignment statement! Variable '" ++ id ++ "' was not defined.")
                where def = getDefinition id scopes
                      t = getTypeFromDef def

initStatement stm@(SmtCall id exps) (Right scopes) =
                    if def /= Left defNotFound
                    then
                        if checkParams exps params scopes
                        then (Right scopes)
                        else Left ("Type mismatch in func call statement! The parameters do not fit the definition!")
                    else Left ("Type error in function call! Function '" ++ id ++ "' was not defined.")
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
                else False
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

checkForkParams exprs scopes =
        and (map isExprVar exprs) &&
        and (map (\(ExprVar id) -> getDefinition id scopes /= Left defNotFound) exprs)

getIdsFromVars [] = []
getIdsFromVars ((ExprVar id):xs) = id : (getIdsFromVars xs)

isExprVar (ExprVar id) = True
isExprVar _ = False

checkForkLocks ids [] = True
checkForkLocks ids ((SmtLock id):statements) = (id `elem` ids) && checkForkLocks ids statements
checkForkLocks ids ((SmtUnlock id):statements) = (id `elem` ids) && checkForkLocks ids statements
checkForkLocks ids ((SmtIf e s1 s2):statements) = checkForkLocks ids s1 && checkForkLocks ids s2 && checkForkLocks ids statements
checkForkLocks ids ((SmtWhile e s):statements) = checkForkLocks ids s && checkForkLocks ids statements
checkForkLocks ids ((SmtFork exprs s1 s2):statements) = checkForkLocks (ids ++ i) s1 && checkForkLocks (ids ++ i) s2 && checkForkLocks ids statements
                    where i = getIdsFromVars exprs
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
initScope :: [Statement] -> Either String [[(String, Statement)]] -> Either String [[(String, Statement)]]
initScope _ (Left err) = Left err
initScope [] (Right scopes) = Right (init scopes)
initScope statements (Right scopes) = if isLeft folded then folded else Right (init (fromRight [[]] folded))
                       where folded = initializeStatements statements (Right scopes)

initializeStatements :: [Statement] -> Either String [[(String, Statement)]] -> Either String [[(String, Statement)]]
initializeStatements [] (Right scopes) = (Right scopes)
initializeStatements (s:statements) (Right scopes) = initializeStatements statements (initStatement s (Right scopes))
initializeStatements (s:statements) (Left err) = Left err


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

fromRight b (Right x) = x
fromRight b (Left _) = b

fromLeft a (Left x) = x
fromLeft a (Right _) = a

----------------- TEST
testFront :: String -> Either String Program
testFront p = if isLeft res then (Left (fromLeft "" res)) else (Right parsed)
        where (Right parsed) = parse parseProgram [] p
              res = testInit (parsed)

testInit p = initProg p (Right [[]])

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
