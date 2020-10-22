module Untyped where

import Control.Exception.Base (Exception)
import Data.Data (Typeable)

type VarName = Int

data FileInfo = MkInfo Int Int

data Term
  = TmVar FileInfo VarName Int
  | TmAbs FileInfo String Term
  | TmApp FileInfo Term Term

type Context = [(String, Binding)]

data Binding = NameBind

shift :: Int -> Term -> Term
shift d = shiftH d 0

shiftH :: Int -> Int -> Term -> Term
shiftH d c (TmVar i n ck) = if n >= c then TmVar i (n + d) (ck + d) else TmVar i n (ck + d)
shiftH d c (TmAbs i s t) = TmAbs i s (shiftH d (c + 1) t)
shiftH d c (TmApp i t1 t2) = TmApp i (shiftH d c t1) (shiftH d c t2)

substitute :: VarName -> Term -> Term -> Term
substitute v r = helper 0
  where
    helper :: Int -> Term -> Term
    helper c (TmVar i n ck) = if n == v + c then shift c r else TmVar i n ck
    helper c (TmAbs i n tt) = TmAbs i n (helper (c + 1) tt)
    helper c (TmApp i t1 t2) = TmApp i (helper c t1) (helper c t2)

termSubsTop :: Term -> Term -> Term
termSubsTop s t = shift (-1) (substitute 0 (shift 1 s) t)

isVal :: Term -> Bool
isVal (TmAbs _ _ _) = True
isVal _ = False

data Stuck = ImStuck deriving (Show, Typeable)

instance Exception Stuck

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
  (TmApp _ (TmAbs _ _ t12) v2) | isVal v2 -> Just (termSubsTop v2 t12)
  (TmApp i v1 t2) | isVal v1 -> do
    t2_ <- eval1 ctx t2
    Just (TmApp i v1 t2_)
  (TmApp i t1 t2) -> do
    t1_ <- eval1 ctx t1
    return (TmApp i t1_ t2)
  _ -> Nothing

eval :: Context -> Term -> Term
eval ctx t = do
  let t_ = eval1 ctx t
  case t_ of
    Nothing -> t
    Just t_n -> eval ctx t_n

instance Show Term where
  show (TmVar _ n _) = show n
  show (TmAbs _ _ t) = "λ." ++ show t
  show (TmApp _ t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

showPrettyNames :: Term -> String
showPrettyNames t = helper t []
  where
    helper :: Term -> [String] -> String
    helper (TmVar _ n _) ov = reverse ov !! n
    helper (TmAbs _ ogname t1) ov = "λ" ++ ogname ++ "." ++ helper t1 (ov ++ [ogname])
    helper (TmApp _ t1 t2) ov = "(" ++ helper t1 ov ++ " " ++ helper t2 ov ++ ")"

dontCareInfo :: FileInfo
dontCareInfo = MkInfo 0 0
