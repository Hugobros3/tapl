module UntypedParse where

import Untyped

import Control.Applicative
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
  _ <- string "("
  t <- parseTerm
  _ <- string ")"
  return t

parseAbs :: ReadP Term
parseAbs = do
  string "λ"
  -- name <- fmap show parseVarName
  string "."
  t <- parseTerm
  return (TmAbs dci "todo_gib_names" t)

parseApp :: ReadP (Maybe Term)
parseApp = do
  pure Nothing <|> smh where
  smh = do
    string " "
    t2 <- parseTerm
    return (Just t2) -- (TmApp dci t1 t2)

parseTerm :: ReadP Term
parseTerm = do
  t <- parseVar <|> parseAbs <|> parseParent
  app <- parseApp
  case app of
    Nothing -> return t
    Just t2 -> return (TmApp dci t t2)
  
parseProgram :: ReadP Term
parseProgram = do parseTerm

readProgram :: String -> Term
readProgram s = fst (last (readP_to_S parseProgram s))