{-# LANGUAGE RecursiveDo #-}

module Interpreter where

import Control.Monad
import Data.Map as M
import SimpleGrammar
import ErrM
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Text.PrettyPrint as PP

type Result = Err Value

data Value = Const Integer | Func (Result -> Result) |
    Variant String [Result]

instance Show DisplayValue where
  show x = show (prValue x)

prValue :: DisplayValue -> PP.Doc
prValue (DisplayConst n) = PP.text (show n)
prValue (DisplayFunc) = PP.text "function"
prValue (DisplayVariant s d) =
  PP.text s PP.<+> PP.hsep (Prelude.map prParenValue d)

prParenValue :: DisplayValue -> PP.Doc
prParenValue t@(DisplayVariant _ d) | length d > 0 = PP.parens (prValue t)
prParenValue t = prValue t

data DisplayValue = DisplayConst Integer
                  | DisplayFunc
                  | DisplayVariant String [DisplayValue];
resultToDisplay :: Result -> Err DisplayValue
resultToDisplay res = do
  val <- res
  case val of
    Const n -> return $ DisplayConst n
    Func _ -> return DisplayFunc
    Variant s d -> do
      displayD <- mapM resultToDisplay d
      return $ DisplayVariant s displayD


type Env = M.Map String (Err Value)

transConstructor :: String -> [a] -> [Result] -> Value
transConstructor name [] results = Variant name results
transConstructor name (_:rest) results =
  Func (\r -> Ok $ transConstructor name rest (results ++ [r]))

-- evalEnv is the fixed point of the environment, the expressions are
-- evaluated in evalEnv. envStub is an accumulator for foldM and this value
-- is updated and then returned.
transDecl :: Env -> Env -> Decl -> Err Env
transDecl evalEnv envStub (DValue (Ident name) expr) =
  Ok $ insert name (transExp evalEnv expr) envStub

transDecl _ envStub (DData _ variants) =
  foldM go envStub variants where
    go :: Env -> Variant -> Err Env
    go env' (Var (Ident name) args) =
      Ok $ insert name (Ok $ transConstructor name args []) env'

transDecl _ envStub (DType _ _) = Ok envStub

transDecls :: Env -> [Decl] -> Err Env
transDecls env decls = do
    rec env' <- foldM (transDecl env') env decls
    return env'

transExp :: Env -> Exp -> Result
transExp env x = case x of
  EInt integer -> Ok $ Const integer
  ELet decls expr -> do
    env' <- transDecls env decls
    transExp env' expr
  EVar (Ident ident) ->
    fromMaybe (Bad $ "identifier " ++ ident ++ " unset") (M.lookup ident env)
  ELambda (Ident ident) expr ->
    Ok (Func func) where
      func arg = transExp env' expr where
        env' = insert ident arg env
  EApp func arg -> do
    funcVal <- transExp env func
    case funcVal of
      Func f -> do
        let argRes = transExp env arg
        f argRes
      _ -> Bad "cannot apply to a constant"
  ECase expr caseParts ->
    let
      matchSubpatterns :: Env -> [Result] -> [Pattern] -> Maybe (Err Env)
      matchSubpatterns env' results patterns =
        if length results /= length patterns
          then Just $ Bad "number of variant args does not match"
          else Prelude.foldl go (Just $ Ok env') (zip results patterns)
            where
              go :: Maybe (Err Env) -> (Result, Pattern) -> Maybe (Err Env)
              go Nothing _ = Nothing
              go (Just (Bad  s)) _ = Just $ Bad s
              go (Just (Ok env'')) (result, pat) = case result of
                Ok value -> matchPattern env'' value pat
                Bad s -> Just (Bad s)

      matchPattern :: Env -> Value -> Pattern -> Maybe (Err Env)
      matchPattern env' _ PAny = Just (Ok env')
      matchPattern env' value (PValue (Ident str)) =
        Just $ Ok $ insert str (Ok value) env'
      matchPattern env' value (PVariant (Ident exprectedName) patterns) =
        case value of
          Variant variantName variantData ->
            if variantName == exprectedName
              then matchSubpatterns env' variantData patterns
              else Nothing
          _ -> Just $ Bad $
            "you cannot match case with non variant value"

      matchCasePart :: Value -> CasePart -> Maybe Result
      matchCasePart value (CaseP pat thenExp) =
        case matchPattern env value pat of
          Just (Ok env') -> Just $ transExp env' thenExp
          Just (Bad b) -> Just (Bad b)
          Nothing -> Nothing
    in do
      val <- transExp env expr
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
      Ok (Func (Ok . Func . checkFunc))
      where
        checkFunc r1 r2 =
          do
            v1 <- r1
            v2 <- r2
            checkIntFunc v1 v2

        checkIntFunc (Const i1) (Const i2) = func i1 i2
        checkIntFunc _ _ = Bad "unsupported non-integer argument for arithmetic operation"

    division = binaryIntFuncWrapper foo
      where
        foo _ i2 | i2 == 0 = Bad "division by 0"
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


interpretWithBuiltins :: Program -> Program -> Err DisplayValue
interpretWithBuiltins (Program builtinsDecls) (Program programDecls) =
    do
      builtinEnv <- transDecls specialBuiltins builtinsDecls
      res <- transExp builtinEnv expr
      resultToDisplay (Ok res)
  where
    expr = ELet programDecls (EVar (Ident "main"))

