module Interpreter where

import Debug.Trace

import Control.Monad
import Data.Map as M
import AbsGrammar
import ErrM
type Result = Err Value

data Value = Const Integer | Func (Value -> Result)

instance Show Value where
  show (Const n) = "Const " ++ (show n)
  show (Func f) = "Func"

type Env = M.Map String Value

arithm :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Result
arithm op env exp1 exp2 = do
  v1 <- transExp env exp1
  v2 <- transExp env exp2
  case (v1, v2) of 
    (Const i1, Const i2) -> Ok $ Const (i1 `op` i2)
    _ -> Bad $ "arithmetic operations only supported for consts"

transDecl :: Env -> Decl -> Err Env
transDecl env (D (Ident name) argsIdents exp) = do
  let composeLambdas argIdent partialExp = ELambda argIdent partialExp
  let func = Prelude.foldr composeLambdas exp argsIdents
  val <- transExp env func
  let env' = insert name val env
  return env'

transExp :: Env -> Exp -> Result
transExp env x = case x of
  EAdd exp1 exp2 -> arithm (+) env exp1 exp2
  ESub exp1 exp2 -> arithm (-) env exp1 exp2
  EMul exp1 exp2 -> arithm (*) env exp1 exp2
  EDiv exp1 exp2 -> Bad "integer division not implemented"
  EInt integer -> Ok $ Const $ integer
  EWhere exp decls -> do
    env' <- foldM transDecl env decls
    v1 <- transExp env' exp
    return v1
  ELet (Ident str) exp1 exp2 -> do
    v1 <- transExp env exp1
    let env' = insert str v1 env
    v2 <- transExp env' exp2
    return v2
  EVar (Ident ident) ->
    maybe (Bad $ "identifier " ++ ident ++ " unset") Ok maybeVal where
      maybeVal :: Maybe Value
      maybeVal = M.lookup ident env
  EIf exp1 exp2 exp3 -> do
    v1 <- transExp env exp1
    case v1 of 
      Const i1 -> 
        if i1 /= 0 then
          transExp env exp2
        else
          transExp env exp3
      _ -> Bad $ "function is not a condition"
  ELambda (Ident ident) exp ->
    Ok (Func func) where
      func arg = transExp env' exp where
        env' = insert ident arg env
  EApp func arg -> do
    funcVal <- transExp env func
    case funcVal of
      Func f -> do
        val <- transExp env arg
        f val
      _ -> Bad "cannot apply to a constant"

interpret = transExp empty
