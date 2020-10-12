module Main where

import Untyped
import UntypedParse

import GHC.IO.Encoding

test = TmAbs dci "x" (TmVar dci 0 1)

parserTest = "λ.λ.(0 1)"

evalTest = "(λ.(0)) (λ.(λ.(1 0)))"

main :: IO ()
main = do
  setLocaleEncoding utf8
  let p = readProgram evalTest
  print p
  ep <- (eval1 [] p)
  print ep
  --putStrLn "Hi"
  --putStrLn "World"
  --print p
  --putStrLn (showPrettyNames p)