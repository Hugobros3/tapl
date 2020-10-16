module Main where

import Untyped
import UntypedParse

import GHC.IO.Encoding

main :: IO ()
main = do
  let test = TmAbs dontCareInfo "x" (TmVar dontCareInfo 0 1)
  let parserTest = "λ.λ.(0 1)"
  let evalTest = "(λ.(0)) (λ.(λ.(1 0)))"

  setLocaleEncoding utf8
  let p = readProgram evalTest
  print p
  let ep = eval1 [] p
  print ep