module Main where
import GHC.IO.Encoding

type VarName = Int

data Info = MkInfo Int Int

data Term
  = TmVar Info VarName Int
  | TmAbs Info String Term
  | TmApp Info Term Term

shift :: Int -> Term -> Term
shift d = shiftH d 0

shiftH :: Int -> Int -> Term -> Term
shiftH d c (TmVar i n ck) = if n >= c then TmVar i (n+d) (ck+d) else TmVar i n (ck+d)
shiftH d c (TmAbs i s t) = TmAbs i s (shiftH d (c + 1) t)
shiftH d c (TmApp i t1 t2) = TmApp i (shiftH d c t1) (shiftH d c t2)

substitute :: VarName -> Term -> Term -> Term
substitute v r = helper 0 where 
    helper :: Int -> Term -> Term
    helper c (TmVar i n ck) = if n == v + c then shift c r else TmVar i n ck
    helper c (TmAbs i n tt) = TmAbs i n (helper (c+1) tt)
    helper c (TmApp i t1 t2) = TmApp i (helper c t1) (helper c t2)

instance Show Term where
  show (TmVar _ n _) = show n
  show (TmAbs _ _ t) = "λ." ++ show t
  show (TmApp _ t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

showPrettyNames :: Term -> String
showPrettyNames t = helper t [] where
   helper :: Term -> [String] -> String
   helper (TmVar _ n _) ov = reverse ov !! n
   helper (TmAbs _ ogname t1) ov = "λ" ++ ogname ++ "." ++ helper t1 (ov++[ogname])
   helper (TmApp _ t1 t2) ov = "(" ++ helper t1 ov ++ " " ++ helper t2 ov ++ ")"

dci = MkInfo 0 0
test = TmAbs dci "x" (TmVar dci 0 1)

main :: IO ()
main = do
 setLocaleEncoding utf8
 let p = test
 putStrLn "Hi"
 putStrLn "World"
 print p
 putStrLn (showPrettyNames p)
