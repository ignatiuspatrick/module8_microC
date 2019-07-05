module Grammar where

import Data.List
import Data.Char
import Data.Functor
import Data.Either

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


data Program = Program [Statement]
                deriving (Show)

data Definition = FunctionDef Type String [Param] [Statement]
                | VariableDef Type String Expression
                deriving (Show, Eq)

-- Variables
data Type = IntType () | BoolType () | VoidType ()
                deriving (Show, Eq)



-- Functions
data Param = Param Type String
                deriving (Show, Eq)


-- Language
data Statement = SmtDef Definition
               | SmtIf Expression [Statement] [Statement]
               | SmtWhile Expression [Statement]
               | SmtRet Expression
               | SmtAss String Expression
               | SmtCall String [Expression]
               | SmtFork [Expression] [Statement] [Statement]
               | SmtLock String
               | SmtUnlock String
                deriving (Show, Eq)


data Order = OrderLT () | OrderLE () | OrderEQ () | OrderNE () | OrderGT () | OrderGE ()
                deriving (Show, Eq)

data Binary = BinaryAnd () | BinaryOr ()
            deriving (Show, Eq)

data Expression = ExprConst Integer
          | ExprTrue ()
          | ExprFalse ()
          | ExprVar String
          | ExprAdd Expression Expression
          | ExprSubtract Expression Expression
          | ExprMult Expression Expression
          | ExprBrac Expression
          | ExprBool Expression Order Expression
          | ExprBin Expression Binary Expression
          | ExprCall String [Expression]
          deriving (Show, Eq)

fromLeft' :: Either l r -> l
fromLeft' (Left x) = x -- Newer GHC versions contain a fromLeft :: l -> Either l r -> l

fromRight' :: Either l r -> r
fromRight' (Right x) = x -- Newer GHC versions contain a fromRight :: r -> Either l r -> r

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
          | otherwise  = fromRight' res
  where res = parse p "" xs

languageDef = emptyDef {
                Token.reservedOpNames = ["=", "+", "*", "-", "==", "!=", "<", "<=", ">", ">=", "&&", "||"],
                Token.reservedNames = ["fork", "lock", "unlock", "", "if", "while", "else", "function", "return", "int", "boolean", "void", "true", "false", ","],
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
whiteSpace = Token.whiteSpace lexer


