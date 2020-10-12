module UntypedParse where

import Untyped
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

-- WIP

digit :: ReadP Char
digit = satisfy isDigit

parseVarName:: ReadP VarName
parseVarName = do fmap read (many1 digit)

parseVar :: ReadP Term
parseVar = do
  n <- parseVarName
  return (TmVar dci n 0)

parseParent :: ReadP Term
parseParent = do
  string "("
  t <- parseTerm
  string ")"
  return t

parseAbs :: ReadP Term
parseAbs = do
  string "λ"
  name <- fmap show parseVarName
  string "."
  t <- parseTerm
  return (TmAbs dci name t)

parseApp :: ReadP Term
parseApp = do
  t1 <- parseTerm
  string " "
  t2 <- parseTerm
  return (TmApp dci t1 t2)

parseTerm :: ReadP Term
parseTerm = do
  return undefined