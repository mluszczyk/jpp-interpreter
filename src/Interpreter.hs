{-# LANGUAGE RecursiveDo #-}

module Interpreter where

import Debug.Trace

import Control.Monad
import Control.Monad.Fix
import Data.Map as M
import AbsGrammar
import ErrM
import Data.Maybe (catMaybes)
type Result = Err Value

data Value = Const Integer | Func (Value -> Result) |
    Variant String [Value]

instance Show Value where
  show (Const n) = "Const " ++ (show n)
  show (Func f) = "Func"
  show (Variant s d) = ("Variant " ++ s ++ (show d))

type Env = M.Map String Value

arithm :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Result
arithm op env exp1 exp2 = do
  v1 <- transExp env exp1
  v2 <- transExp env exp2
  case (v1, v2) of 
    (Const i1, Const i2) -> Ok $ Const (i1 `op` i2)
    _ -> Bad $ "arithmetic operations only supported for consts"

instance MonadFix (Err) where
    mfix f = let a = f (unRight a) in a
             where unRight (Ok x) = x
                   unRight (Bad _) = errorWithoutStackTrace "mfix Either: Left"

transConstructor :: String -> [a] -> [Value] -> Value
transConstructor name [] values = Variant name values
transConstructor name (arg:rest) values =
  Func (\v -> Ok $ transConstructor name rest (values ++ [v]))

transDecl :: Env -> Decl -> Err Env
transDecl env (DConst (Ident name) argsIdents exp) =
  let composeLambdas argIdent partialExp = ELambda argIdent partialExp in
  let func = Prelude.foldr composeLambdas exp argsIdents
  in do
      rec val <- transExp (insert name val env) func
      return $ insert name val env

transDecl env (DData declIgnored variants) =
  foldM go env variants where
    go :: Env -> Variant -> Err Env
    go env' (Var (Ident name) args) =
      Ok $ insert name (transConstructor name args []) env'

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
    rec v1 <- transExp (insert str v1 env) exp1
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
  ECase exp caseParts ->
    let 
      matchPattern :: Value -> Pattern -> Maybe (Err Env)
      matchPattern value PAny = Just (Ok env)
      matchPattern value (PVariant (Ident expectedName) patterns) =
        case value of
          Variant variantName variantData -> 
            if variantName == expectedName then Just $ Ok env else Nothing
          _ -> Just $ Bad $ 
            "you cannot match case with non variant value" ++ (show value)

      matchCasePart :: Value -> CasePart -> Maybe Result
      matchCasePart value (CaseP pattern thenExp) = 
        case matchPattern value pattern of
          Just (Ok env') -> Just $ transExp env' thenExp
          Just (Bad b) -> Just (Bad b)
          Nothing -> Nothing
    in do
      val <- transExp env exp
      let matches = catMaybes $ Prelude.map (matchCasePart val) caseParts
      case matches of 
        [] -> Bad "exhausted pattern matching"
        (a:_) -> a

interpret = transExp empty
