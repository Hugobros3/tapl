module Simple where

import Control.Exception.Base (try)

type VarName = Int

data FileInfo = MkInfo Int Int

type Context = [(String, Binding)]

data Binding = NameBind | VarBind Type

data Type = TyBool | TyArrow Type Type deriving (Eq, Show)

data Term
  = TmTrue FileInfo
  | TmFalse FileInfo
  | TmIf FileInfo Term Term Term
  | TmVar FileInfo VarName Int
  | TmAbs FileInfo String Type Term
  | TmApp FileInfo Term Term

addBinding :: Context -> String -> Binding -> Context
addBinding ctx vn bind = (vn, bind) : ctx

getBinding :: FileInfo -> Context -> VarName -> Binding
getBinding _ ctx i = snd (ctx!!i)

getTypeFromCtx :: FileInfo -> Context -> VarName -> Type
getTypeFromCtx info ctx i = case getBinding info ctx i of
  VarBind ty -> ty
  _ -> error "Good job kevin"

-- Type checking

newtype TypeError = TypeError String

typeof :: Context -> Term -> Either TypeError Type
typeof _ (TmTrue  _) = return TyBool
typeof _ (TmFalse _) = return TyBool
typeof ctx (TmIf fi condition ifTrue ifFalse) = do
  conditionType <- typeof ctx condition
  if conditionType /= TyBool then Left (TypeError ("Condition type is not a bool, is actually " ++ show conditionType)) else do
    ifTrueType <- typeof ctx ifTrue
    ifFalseType <- typeof ctx ifFalse
    if ifTrueType /= ifFalseType then Left(TypeError ("Types for the if branches do not match: true branch is " ++ show ifTrueType ++ " while false branch is " ++ show ifFalseType)) else do
      return ifTrueType
typeof ctx (TmVar fi v _) = return (getTypeFromCtx fi ctx v)
typeof ctx (TmAbs fi name tT t2) = do
  let ext_ctx = addBinding ctx name (VarBind tT)
  t2T <- typeof ext_ctx t2
  return (TyArrow tT t2T)
typeof ctx (TmApp fi t1 t2) = do
  t1T <- typeof ctx t1
  t2T <- typeof ctx t2
  case t1T of
    (TyArrow fromT toT) -> if fromT == t2T then return toT else
      Left (TypeError ("Function type" ++ show (TyArrow fromT toT) ++ " takes a " ++ show fromT ++ " but was given a " ++ show t2T))
    _ -> Left (TypeError ("Not a function type"))