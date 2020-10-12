module Main where

import Untyped
import UntypedParse

import GHC.IO.Encoding

test = TmAbs dci "x" (TmVar dci 0 1)

parserTest = "λ.λ.(0 1)"

main :: IO ()
main = do
  setLocaleEncoding utf8
  let p = readProgram parserTest
  putStrLn "Hi"
  putStrLn "World"
  print p
  putStrLn (showPrettyNames p)