{-# LANGUAGE RecursiveDo #-}

module Interpreter where

import Debug.Trace

import Control.Monad
import Control.Monad.Fix
import Data.Map as M
import SimpleGrammar
import ErrM
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
type Result = Err Value

data Value = Const Integer | Func (Value -> Result) |
    Variant String [Value]

instance Show Value where
  show (Const n) = "Const " ++ show n
  show (Func f) = "Func"
  show (Variant s d) = "Variant " ++ s ++ show d

type Env = M.Map String (Err Value)

instance MonadFix Err where
    mfix f = let a = f (unRight a) in a
             where unRight (Ok x) = x
                   unRight (Bad _) = errorWithoutStackTrace "mfix Either: Left"

transConstructor :: String -> [a] -> [Value] -> Value
transConstructor name [] values = Variant name values
transConstructor name (arg:rest) values =
  Func (\v -> Ok $ transConstructor name rest (values ++ [v]))

transDecl :: Env -> Env -> Decl -> Err Env
transDecl evalEnv envStub (DValue (ValueIdent name) argsIdents exp) =
  let func = Prelude.foldr ELambda exp argsIdents
  in Ok $ insert name (transExp evalEnv func) envStub

transDecl evalEnv envStub (DData declIgnored variants) =
  foldM go envStub variants where
    go :: Env -> Variant -> Err Env
    go env' (Var (TypeIdent name) args) =
      Ok $ insert name (Ok $ transConstructor name args []) env'

transDecl _ envStub (DType _ _) = Ok envStub

transDecls :: Env -> [Decl] -> Err Env
transDecls env decls = do
    rec env' <- foldM (transDecl env') env decls
    return env'

valsEqual :: Value -> Value -> Err Bool
valsEqual val1 val2 = 
  case (val1, val2) of
    (Const i1, Const i2) -> Ok (i1 == i2)
    (Variant s1 d1, Variant s2 d2) -> do
      blist <- zipWithM valsEqual d1 d2
      return $ s1 == s2 && length d1 == length d2 && and blist
    (_, _) -> Bad "uncomparable types"

transExp :: Env -> Exp -> Result
transExp env x = case x of
  EInt integer -> Ok $ Const integer
  ELet decls exp -> do
    env' <- transDecls env decls
    transExp env' exp
  EVarValue (ValueIdent ident) ->
    fromMaybe (Bad $ "identifier " ++ ident ++ " unset") (M.lookup ident env)
  EVarType (TypeIdent ident) ->
    fromMaybe (Bad $ "identifier " ++ ident ++ " unset") (M.lookup ident env)
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
        if length values /= length patterns
          then Just $ Bad "number of variant args does not match"
          else Prelude.foldl go (Just $ Ok env') (zip values patterns) 
            where 
              go :: Maybe (Err Env) -> (Value, Pattern) -> Maybe (Err Env)
              go Nothing _ = Nothing
              go (Just (Bad  s)) _ = Just $ Bad s
              go (Just (Ok env'')) (value, pat) =
                matchPattern env'' value pat

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
            "you cannot match case with non variant value" ++ show value

      matchCasePart :: Value -> CasePart -> Maybe Result
      matchCasePart value (CaseP pat thenExp) = 
        case matchPattern env value pat of
          Just (Ok env') -> Just $ transExp env' thenExp
          Just (Bad b) -> Just (Bad b)
          Nothing -> Nothing
    in do
      val <- transExp env exp
      let matches = Data.Maybe.mapMaybe (matchCasePart val) caseParts
      case matches of 
        [] -> Bad "exhausted pattern matching"
        (a:_) -> a

specialBuiltins :: Env
specialBuiltins = fromList 
  [ ("+", arithmBuiltin (+))
  , ("-", arithmBuiltin (-))
  , ("*", arithmBuiltin (*))
  , ("`div`", division)
  , ("==", boolBuiltin (==))
  , ("/=", boolBuiltin (/=))
  , ("<=", boolBuiltin (<=))
  , ("<", boolBuiltin (<))
  , (">=", boolBuiltin (>=))
  , (">", boolBuiltin (>))
  ]
  where
    binaryIntFuncWrapper :: (Integer -> Integer -> Result) -> Result
    binaryIntFuncWrapper func =
      (Ok (Func (\v1 -> Ok $ Func (\v2 -> checkFunc v1 v2))))
      where 
        checkFunc (Const i1) (Const i2) = func i1 i2
        checkFunc _ _ = Bad $ "unsupported non-integer argument for arithmetic operation"

    division = binaryIntFuncWrapper foo
      where 
        foo _ i2 | i2 == 0 = Bad $ "division by 0"
        foo i1 i2 = Ok $ Const (i1 `div` i2)
  
    arithmBuiltin :: (Integer -> Integer -> Integer) -> Result
    arithmBuiltin op = binaryIntFuncWrapper foo
      where
        foo i1 i2 = Ok $ Const (i1 `op` i2)

    boolBuiltin :: (Integer -> Integer -> Bool) -> Result
    boolBuiltin op = binaryIntFuncWrapper foo
      where 
        foo i1 i2  = Ok $ boolToLang (i1 `op` i2)

    boolToLang :: Bool -> Value
    boolToLang False = Variant "False" []
    boolToLang True = Variant "True" []


interpretWithBuiltins :: Program -> Program -> Result
interpretWithBuiltins (Program builtinsDecls) (Program programDecls) =
    do
      builtinEnv <- transDecls specialBuiltins builtinsDecls
      transExp builtinEnv exp
  where
    exp = ELet programDecls (EVarValue (ValueIdent "main"))

