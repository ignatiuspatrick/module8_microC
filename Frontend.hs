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
               try (VariableDef <$> parseType <*> (identifier <* symbol "=") <*> (parseExpression <* semi))
               <|> try (FunctionDef <$> (reserved "function" *> parseType) <*> identifier <*> parens (commaSep parseParam) <*> braces (many parseStatement))
               <|> GlobalDef <$> (reserved "global" *> (commaSep identifier)) <* semi

-- Variables
parseType = (IntType <$> reserved "int") <|> (BoolType <$> reserved "boolean") <|> (VoidType <$> reserved "void")

-- Functions
parseParam = Param <$> parseType <*> identifier

-- Language
parseStatement =
               SmtIf <$> (reserved "if" *> (parens parseExpression)) <*> (braces (many parseStatement)) <*> (reserved "else" *> (braces (many parseStatement)))
              <|> SmtWhile <$> (reserved "while" *> (parens parseExpression)) <*> (braces (many parseStatement))
              <|> SmtRet <$> (reserved "return" *> parseExpression <* semi)
              <|> SmtDef <$> parseDefinition
              <|> SmtAss <$> (identifier <* symbol "=") <*> parseExpression <* semi
              <|> SmtCall <$> identifier <*> (parens (commaSep parseExpression)) <* semi



-- Algebra

parseExpression =
                try (ExprBool <$> parseStuff <*> parseOrder <*> parseStuff)
                <|> parseStuff


parseStuff = try (ExprAdd <$> (parseTerm <* symbol "+")  <*> parseTerm)
           <|> try (ExprSubtract <$> (parseTerm <* symbol "-")  <*> parseTerm)
           <|> parseTerm

parseTerm = try (ExprMult <$> (parseFact <* symbol "*")  <*> parseFact)
        <|> parseFact

parseFact = trace("parseFact") $
            (ExprConst <$> integer)
            <|> (ExprTrue <$> reserved "true")
            <|> (ExprFalse <$> reserved "false")
            <|> try (ExprCall <$> identifier <*> (parens (commaSep parseExpression)))
            <|> (ExprVar <$> identifier)
            <|> (ExprBrac <$> (parens parseExpression))

parseOrder = OrderLT <$> reserved "<"
          <|> OrderLE <$> reserved "<="
          <|> OrderEQ <$> reserved "=="
          <|> OrderNE <$> reserved "!="
          <|> OrderGT <$> reserved ">"
          <|> OrderGE <$> reserved ">="
