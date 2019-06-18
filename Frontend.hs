module Frontend where

import Data.List
import Data.Char

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Grammar



-- Programs
parseProgram = Program <$> (many parseDefinition)
parseDefinition = FunctionDef <$> parseType <*> identifier <*> parens (commaSep parseParam) <*> (many parseStatement)
               <|> VariableDef <$> parseType <*> identifier <*> parseExpression <* semi
               <|> GlobalDef <$> (commaSep identifier) <* semi

-- Variables
parseType = (IntType <$> reserved "int") <|> (BoolType <$> reserved "boolean") <|> (VoidType <$> reserved "void")

-- Functions
parseParam = Param <$> parseType <*> identifier

-- Language
parseStatement = SmtDef <$> parseDefinition
              <|> SmtIf <$> (reserved "if" *> (parens parseExpression)) <*> (braces (many parseStatement)) <*> (reserved "else" *> (braces (many parseStatement)))
              <|> SmtWhile <$> (reserved "while" *> (parens parseExpression)) <*> (braces (many parseStatement))
              <|> SmtRet <$> parseExpression
              <|> SmtAss <$> identifier <* symbol "=" <*> parseExpression
              <|> SmtCall <$> identifier <*> (parens (commaSep parseExpression))



-- Algebra
add' = (\_ -> (ExprAdd)) <$> symbol "+"

sub' = (\_ -> (ExprSubtract)) <$> symbol "-"

mult' = (\_ -> (ExprMult)) <$> symbol "*"

parseExpression = parseTerm `chainr1` add' <|> parseTerm `chainr1` sub'

parseTerm = parseFact `chainr1` mult'

parseFact = (ExprConst <$> integer)
            <|> try (ExprCall <$> identifier <*> (parens (commaSep parseExpression)))
            <|> (ExprVar <$> identifier)
            <|> (ExprBrac <$> (parens parseExpression))
            <|> (ExprBool <$> parseExpression <*> parseOrder <*> parseExpression)

parseOrder = OrderLT <$> reserved "<"
          <|> OrderLE <$> reserved "<="
          <|> OrderEQ <$> reserved "=="
          <|> OrderNE <$> reserved "!="
          <|> OrderGT <$> reserved ">"
          <|> OrderGE <$> reserved ">="
