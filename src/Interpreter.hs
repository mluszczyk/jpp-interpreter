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

type Env = M.Map String (Err Value)

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

transDecl :: Env -> Env -> Decl -> Err Env
transDecl evalEnv envStub (DValue (ValueIdent name) argsIdents exp) =
  let composeLambdas argIdent partialExp = ELambda argIdent partialExp in
  let func = Prelude.foldr composeLambdas exp argsIdents
  in Ok $ insert name (transExp evalEnv func) envStub

transDecl evalEnv envStub (DData declIgnored variants) =
  foldM go envStub variants where
    go :: Env -> Variant -> Err Env
    go env' (Var (TypeIdent name) args) =
      Ok $ insert name (Ok $ transConstructor name args []) env'
    go env' (SimpleVar (TypeIdent name)) =
      Ok $ insert name (Ok $ transConstructor name [] []) env'

transDecls :: Env -> [Decl] -> Err Env
transDecls env decls = do
    rec env' <- foldM (transDecl env') env decls
    return env'

transExp :: Env -> Exp -> Result
transExp env x = case x of
  EAdd exp1 exp2 -> arithm (+) env exp1 exp2
  ESub exp1 exp2 -> arithm (-) env exp1 exp2
  EMul exp1 exp2 -> arithm (*) env exp1 exp2
  EDiv exp1 exp2 -> Bad "integer division not implemented"
  EInt integer -> Ok $ Const $ integer
  ELet decls exp -> do
    env' <- transDecls env decls
    v1 <- transExp env' exp
    return v1
  EVarValue (ValueIdent ident) ->
    maybe (Bad $ "identifier " ++ ident ++ " unset") id (M.lookup ident env)
  EVarType (TypeIdent ident) ->
    maybe (Bad $ "identifier " ++ ident ++ " unset") id (M.lookup ident env)
  EIf exp1 exp2 exp3 -> do
    v1 <- transExp env exp1
    case v1 of 
      Const i1 -> 
        if i1 /= 0 then
          transExp env exp2
        else
          transExp env exp3
      _ -> Bad $ "function is not a condition"
  ELambda (ValueIdent ident) exp ->
    Ok (Func func) where
      func arg = transExp env' exp where
        env' = insert ident (Ok arg) env
  EApp func arg -> do
    funcVal <- transExp env func
    case funcVal of
      Func f -> do
        val <- transExp env arg
        f val
      _ -> Bad "cannot apply to a constant"
  ECase exp caseParts ->
    let 
      matchSubpatterns :: Env -> [Value] -> [Pattern] -> Maybe (Err Env)
      matchSubpatterns env' values patterns =
        if (length values) /= (length patterns)
          then Just $ Bad $ "number of variant args does not match"
          else Prelude.foldl go (Just $ Ok env') (zip values patterns) 
            where 
              go :: (Maybe (Err Env)) -> (Value, Pattern) -> (Maybe (Err Env))
              go Nothing _ = Nothing
              go (Just (Bad  s)) _ = Just $ Bad $ s
              go (Just (Ok env'')) (value, pattern) =
                matchPattern env'' value pattern

      matchPattern :: Env -> Value -> Pattern -> Maybe (Err Env)
      matchPattern env' value PAny = Just (Ok env')
      matchPattern env' value (PValue (ValueIdent str)) =
        Just $ Ok $ insert str (Ok value) env'
      matchPattern env' value (PVariant (TypeIdent expectedName) patterns) =
        case value of
          Variant variantName variantData -> 
            if variantName == expectedName
              then matchSubpatterns env' variantData patterns
              else Nothing
          _ -> Just $ Bad $ 
            "you cannot match case with non variant value" ++ (show value)

      matchCasePart :: Value -> CasePart -> Maybe Result
      matchCasePart value (CaseP pattern thenExp) = 
        case matchPattern env value pattern of
          Just (Ok env') -> Just $ transExp env' thenExp
          Just (Bad b) -> Just (Bad b)
          Nothing -> Nothing
    in do
      val <- transExp env exp
      let matches = catMaybes $ Prelude.map (matchCasePart val) caseParts
      case matches of 
        [] -> Bad "exhausted pattern matching"
        (a:_) -> a

interpret :: Program -> Result
interpret (Program decls) = do
  env <- transDecls empty decls
  val <- transExp env (EVarValue (ValueIdent "main"))
  return val

