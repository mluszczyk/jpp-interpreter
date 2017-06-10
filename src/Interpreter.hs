{-# LANGUAGE RecursiveDo #-}

module Interpreter where

import Control.Monad
import Data.Map as M
import SimpleGrammar
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Text.PrettyPrint as PP

type PResult a = Either InterpreterError a
type Result = PResult Value

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
resultToDisplay :: Result -> PResult DisplayValue
resultToDisplay res = do
  val <- res
  case val of
    Const n -> return $ DisplayConst n
    Func _ -> return DisplayFunc
    Variant s d -> do
      displayD <- mapM resultToDisplay d
      return $ DisplayVariant s displayD


type Env = M.Map String Result

transConstructor :: String -> [a] -> [Result] -> Value
transConstructor name [] results = Variant name results
transConstructor name (_:rest) results =
  Func (\r -> Right $ transConstructor name rest (results ++ [r]))

-- evalEnv is the fixed point of the environment, the expressions are
-- evaluated in evalEnv. envStub is an accumulator for foldM and this value
-- is updated and then returned.
transDecl :: Env -> Env -> Decl -> PResult Env
transDecl evalEnv envStub (DValue (Ident name) expr) =
  Right $ insert name (transExp evalEnv expr) envStub

transDecl _ envStub (DData _ variants) =
  foldM go envStub variants where
    go :: Env -> Variant -> PResult Env
    go env' (Var (Ident name) args) =
      Right $ insert name (Right $ transConstructor name args []) env'

transDecl _ envStub (DType _ _) = Right envStub

transDecls :: Env -> [Decl] -> PResult Env
transDecls env decls = do
    rec env' <- foldM (transDecl env') env decls
    return env'


data InterpreterError =
    IdentifierUnset String
  | AppToConstant
  | IncorrectNumOfVariantParameters
  | CaseWithNonVariant
  | ExhaustedPatternMatching
  | NonIntegerForArithmeticOperation
  | DivisionByZero

errorMessage :: InterpreterError -> String
errorMessage (IdentifierUnset ident) =
  "identifier " ++ ident ++ " unset"
errorMessage AppToConstant =
  "cannot apply to a constant"
errorMessage IncorrectNumOfVariantParameters =
  "number of variant args does not match"
errorMessage CaseWithNonVariant =
  "you cannot match case with non variant value"
errorMessage ExhaustedPatternMatching =
  "exhausted pattern matching"
errorMessage NonIntegerForArithmeticOperation =
  "unsupported non-integer argument for arithmetic operation"
errorMessage DivisionByZero =
  "division by 0"

instance Show InterpreterError where
  show s = errorMessage s

transExp :: Env -> Exp -> Result
transExp env x = case x of
  EInt integer -> Right $ Const integer
  ELet decls expr -> do
    env' <- transDecls env decls
    transExp env' expr
  EVar (Ident ident) ->
    fromMaybe (Left $ IdentifierUnset ident) (M.lookup ident env)
  ELambda (Ident ident) expr ->
    Right (Func func) where
      func arg = transExp env' expr where
        env' = insert ident arg env
  EApp func arg -> do
    funcVal <- transExp env func
    case funcVal of
      Func f -> do
        let argRes = transExp env arg
        f argRes
      _ -> Left AppToConstant
  ECase expr caseParts ->
    let
      matchSubpatterns :: Env -> [Result] -> [Pattern] -> Maybe (PResult Env)
      matchSubpatterns env' results patterns =
        if length results /= length patterns
          then Just $ Left IncorrectNumOfVariantParameters
          else Prelude.foldl go (Just $ Right env') (zip results patterns)
            where
              go :: Maybe (PResult Env) -> (Result, Pattern) -> Maybe (PResult Env)
              go Nothing _ = Nothing
              go (Just (Left  s)) _ = Just $ Left s
              go (Just (Right env'')) (result, pat) = case result of
                Right value -> matchPattern env'' value pat
                Left s -> Just (Left s)

      matchPattern :: Env -> Value -> Pattern -> Maybe (PResult Env)
      matchPattern env' _ PAny = Just (Right env')
      matchPattern env' value (PValue (Ident str)) =
        Just $ Right $ insert str (Right value) env'
      matchPattern env' value (PVariant (Ident exprectedName) patterns) =
        case value of
          Variant variantName variantData ->
            if variantName == exprectedName
              then matchSubpatterns env' variantData patterns
              else Nothing
          _ -> Just $ Left CaseWithNonVariant

      matchCasePart :: Value -> CasePart -> Maybe Result
      matchCasePart value (CaseP pat thenExp) =
        case matchPattern env value pat of
          Just (Right env') -> Just $ transExp env' thenExp
          Just (Left b) -> Just (Left b)
          Nothing -> Nothing
    in do
      val <- transExp env expr
      let matches = Data.Maybe.mapMaybe (matchCasePart val) caseParts
      case matches of
        [] -> Left ExhaustedPatternMatching
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
      Right (Func (Right . Func . checkFunc))
      where
        checkFunc r1 r2 =
          do
            v1 <- r1
            v2 <- r2
            checkIntFunc v1 v2

        checkIntFunc (Const i1) (Const i2) = func i1 i2
        checkIntFunc _ _ = Left NonIntegerForArithmeticOperation

    division = binaryIntFuncWrapper foo
      where
        foo _ i2 | i2 == 0 = Left DivisionByZero
        foo i1 i2 = Right $ Const (i1 `div` i2)

    arithmBuiltin :: (Integer -> Integer -> Integer) -> Result
    arithmBuiltin op = binaryIntFuncWrapper foo
      where
        foo i1 i2 = Right $ Const (i1 `op` i2)

    boolBuiltin :: (Integer -> Integer -> Bool) -> Result
    boolBuiltin op = binaryIntFuncWrapper foo
      where
        foo i1 i2  = Right $ boolToLang (i1 `op` i2)

    boolToLang :: Bool -> Value
    boolToLang False = Variant "False" []
    boolToLang True = Variant "True" []


interpretWithBuiltins :: Program -> Program -> PResult DisplayValue
interpretWithBuiltins (Program builtinsDecls) (Program programDecls) =
    do
      builtinEnv <- transDecls specialBuiltins builtinsDecls
      res <- transExp builtinEnv expr
      resultToDisplay (Right res)
  where
    expr = ELet programDecls (EVar (Ident "main"))

