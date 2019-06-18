module Grammar where

import Data.List
import Data.Char
import Data.Functor
import Data.Either

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


{-

program : definition *;

definition : functionDef
           | variableDef SEMI
           | globalDef SEMI;

// Variables
variableDef : type identifier ASS expression;

array : LSQBRACKET (item ',') * item RSQBRACKET;

type: BOOLTYPE | INTTYPE | ARRTYPE;

globalDef: GLOBAL (identifier ',')* identifier;

// Functions
functionDef : FUNCTION retType identifier LPAR params RPAR LBRACK statement+ RBRACK;

retType: type | VOID;

params: ((type identifier ',')* type identifier)*;


// Language
statement : defintion
          | if
          | while
          | RETURN identifier
          | expression;

if: IF LPAR bool RPAR LBRACK statement * RBRACK (ELSE LBRACK statement * RBRACK )*;

while: WHILE LPAR bool RPAR LBRACK statement * RBRACK;

bool: expression ordering expression | identifier;

ordering: LT | LE | EQ | NE | GT | GE;


expression : term
     | term ( '+' | '-' ) expression;

term : factor
     | factor '*' term;

factor : INTEGER | BOOLEAN | array
       | identifier LPAR ((arg ',')* arg) * RPAR
       | identifier
       | '(' expr ')';

-}

data Program = Program [Statement]
                deriving (Show)

data Definition = FunctionDef Type String [Param] [Statement]
                | VariableDef Type String Expression
                | GlobalDef [String]
                deriving (Show)

-- Variables
data Type = IntType () | BoolType () | VoidType ()
                deriving (Show)



-- Functions
data Param = Param Type String
                deriving (Show)


-- Language
data Statement = SmtDef Definition
               | SmtIf Expression [Statement] [Statement]
               | SmtWhile Expression [Statement]
               | SmtRet Expression
               | SmtAss String Expression
               | SmtCall String [Expression]
                deriving (Show)


data Order = OrderLT () | OrderLE () | OrderEQ () | OrderNE () | OrderGT () | OrderGE ()
                deriving (Show)

data Expression = ExprConst Integer
          | ExprTrue ()
          | ExprFalse ()
          | ExprVar String
          | ExprAdd Expression Expression
          | ExprSubtract Expression Expression
          | ExprMult Expression Expression
          | ExprBrac Expression
          | ExprBool Expression Order Expression
          | ExprCall String [Expression]
          deriving Show

-- data Expression =
--                   ExprTerm Term
--                 | ExprPlus Term Expression
--                 | ExprSub Term Expression
--                 | ExprBool Order Term Expression
--                 deriving (Show)
--
-- data Term = TermFact Factor
--           | TermMult Factor Term
--                 deriving (Show)
--
-- data Factor = FactorInt Integer
--             | FactorTrue ()
--             | FactorFalse ()
--             | FactorCall String [Expression]
--             | FactorId String
--             | FactorBrack Expression
--                 deriving (Show)

fromLeft' :: Either l r -> l
fromLeft' (Left x) = x -- Newer GHC versions contain a fromLeft :: l -> Either l r -> l

fromRight' :: Either l r -> r
fromRight' (Right x) = x -- Newer GHC versions contain a fromRight :: r -> Either l r -> r

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
          | otherwise  = fromRight' res
  where res = parse p "" xs

languageDef = emptyDef {
                Token.reservedOpNames = ["=", "+", "*", "-", "==", "!=", "<", "<=", ">", ">="],
                Token.reservedNames = ["if", "while", "else", "function", "return", "int", "boolean", "true", "false", ","],
                Token.identStart = letter,
                Token.identLetter = alphaNum,
                Token.commentLine = "//"
              }

lexer = Token.makeTokenParser languageDef

symbol = Token.symbol lexer
parens = Token.parens lexer
braces = Token.braces lexer
identifier = Token.identifier lexer
integer = Token.integer lexer
reserved = Token.reserved lexer
commaSep = Token.commaSep lexer
semi = Token.semi lexer


