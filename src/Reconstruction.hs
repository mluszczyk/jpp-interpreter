{-
Algorithm W according to Martin Grabmuller paper, but heavily modified.
The original version is available as JPP course material.
-}

module Reconstruction where

import Data.Maybe ( fromMaybe, catMaybes )
import Data.List ( nub, delete, intercalate )
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.State

import qualified Text.PrettyPrint as PP

import SimpleGrammar


intVerboseName :: String
intVerboseName = "Integer"

newtype Lit     =  LInt Integer
             deriving (Eq, Ord)

data Type    =  TVar String
             |  TInt
             |  TFun Type Type
             |  TVariant String [Type]
             deriving (Eq, Ord)

-- type with "forall" variables
-- note, that there may be some other free type variables, that should bind
-- to the env (see function generalize)
data Scheme  =  Scheme [String] Type

class Types a where
    -- free type variables
    ftv    ::  a -> Set.Set String
    -- apply type substitution to the type
    apply  ::  Subst -> a -> a

instance Types Type where
    ftv (TVar n)      =  Set.singleton n
    ftv TInt          =  Set.empty
    ftv (TFun t1 t2)  =  ftv t1 `Set.union` ftv t2
    ftv (TVariant _ params)  =  foldl Set.union Set.empty (map ftv params)

    apply s (TVar n)      =  fromMaybe (TVar n) (Map.lookup n s)
    apply s (TFun t1 t2)  =  TFun (apply s t1) (apply s t2)
    apply _ TInt             =  TInt
    apply s (TVariant name types) = TVariant name (map (apply s) types)

instance Types Scheme where
    ftv (Scheme vars t)      =  ftv t `Set.difference` Set.fromList vars

    apply s (Scheme vars t)  =  Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv     =  foldr (Set.union . ftv) Set.empty

type Subst = Map.Map String Type

nullSubst  ::  Subst
nullSubst  =   Map.empty

composeSubst         :: Subst -> Subst -> Subst
composeSubst s1 s2   = Map.map (apply s1) s2 `Map.union` s1

{- type environment
   varsMap stores the mapping from variable name as it appears in the code to
   its type
   variantsMap for each variant type constructor stores a function that generates
   type of the constructor and types of the constructor parameters with respect
   to freshly generates type variables
-}
type NewConstructor = (() -> TI (Type, [Type]))
data TypeEnv = TypeEnv
  { varsMap :: Map.Map String Scheme
  , variantsMap :: Map.Map String NewConstructor
  , typesMap :: Map.Map String Int
  }

instance Types TypeEnv where
    ftv env      =  ftv (Map.elems (varsMap env))
    apply s env  =  TypeEnv { varsMap = Map.map (apply s) (varsMap env)
                            , variantsMap = variantsMap env
                            , typesMap = typesMap env}

-- converts a type to a scheme by fetching all variables not bound by env
generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList (ftv t `Set.difference` ftv env)

-- tiSupply - variable counter used to create new varaibles
newtype TIState = TIState { tiSupply :: Int }

-- state is for the variable counter
-- except is for unification errors
type TI a = ExceptT String (State TIState) a

runTI :: TI a -> (Either String a, TIState)
runTI t =
    runState (runExceptT t) initTIState
  where initTIState = TIState{tiSupply = 0}

-- create type variable
newTyVar :: () -> TI Type
newTyVar _ =
    do  s <- get
        put s{tiSupply = tiSupply s + 1}
        return (TVar  ("a" ++ show (tiSupply s)))

-- creates a type from a scheme with freshly generated type variables
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do  nvars <- mapM (\ _ -> newTyVar ()) vars
                                  let s = Map.fromList (zip vars nvars)
                                  return $ apply s t

-- unification
-- it returns a substitution which, if applied to both types, makes them equal
mgu :: Type -> Type -> TI Subst
mgu (TFun l r) (TFun l' r')  =  do  s1 <- mgu l l'
                                    s2 <- mgu (apply s1 r) (apply s1 r')
                                    return (s1 `composeSubst` s2)
mgu (TVar u) t               =  varBind u t
mgu t (TVar u)               =  varBind u t
mgu TInt TInt                =  return nullSubst
mgu (TVariant typeName1 params1) (TVariant typeName2 params2)
  | typeName1 == typeName2   =
      -- assert lenghts equal!
      let go subst (param1, param2) =
            do subst' <- mgu param1 param2
               return (subst `composeSubst` subst')
      in foldM go nullSubst (zip params1 params2)

  | otherwise = throwError $ "different variant types do not unfiy: " ++
                              typeName1 ++ " vs. " ++ show typeName2
mgu t1 t2                    =  throwError $ "types do not unify: " ++ show t1 ++
                                " vs. " ++ show t2

-- bind variable and return substitution, don't bind to self
-- throw error if the variable is inside the other type
varBind :: String -> Type -> TI Subst
varBind u t  | t == TVar u           =  return nullSubst
             | u `Set.member` ftv t  =  throwError $ "occurs check fails: " ++ u ++
                                         " vs. " ++ show t
             | otherwise             =  return (Map.singleton u t)

-- infer type of a literal
tiLit :: Lit -> TI (Subst, Type)
tiLit (LInt _)   =  return (nullSubst, TInt)

-- infer type of exp
ti        ::  TypeEnv -> Exp -> TI (Subst, Type)
ti env (EVar (Ident ident)) =
    case Map.lookup ident (varsMap env) of
       Nothing     ->  throwError $ "unbound variable: " ++ ident
       Just sigma  ->  do  t <- instantiate sigma
                           return (nullSubst, t)

ti _ (EInt integer) = tiLit (LInt integer)

ti env (ELambda (Ident n) e) =
  do  tv <- newTyVar ()
      env' <- addScheme env (n, (Scheme [] tv))
      (s1, t1) <- ti env' e
      return (s1, TFun (apply s1 tv) t1)

ti env (EApp e1 e2) =
    do  tv <- newTyVar ()
        (s1, t1) <- ti env e1
        (s2, t2) <- ti (apply s1 env) e2
        s3 <- mgu (apply s2 t1) (TFun t2 tv)
        return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)

ti env (ELet decls e) =
    do  (s1, env1) <- tiDecls env decls
        (s2, t) <- ti (apply s1 env1) e
        return (s1 `composeSubst` s2, t)

ti env (ECase expr caseParts) = do
    (subst, exprType) <- ti env expr
    resultType <- newTyVar ()
    (s, _, t) <- foldM go (subst, exprType, resultType) caseParts
    return (s, t)

  where
    go (s1, exprType, resultType) (CaseP patt result) = do
          (patternType, varMap, s2) <- casePartToType env patt s1
          s3 <- mgu patternType (apply (s2 `composeSubst` s1) exprType)
          let schemeUpdate = Map.map (Scheme []) (Map.map (apply s3) varMap)
          localEnv <- foldM addScheme env (Map.toList schemeUpdate)
          (s4, resultType') <- ti (apply (s3 `composeSubst` s2 `composeSubst` s1) localEnv)
              result
          s5 <- mgu (apply (
            s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
            ) resultType) resultType'
          return (s5 `composeSubst` s4 `composeSubst`
                  s3 `composeSubst` s2 `composeSubst` s1
                 , apply (s5 `composeSubst` s4 `composeSubst`
                          s3 `composeSubst` s2 `composeSubst`
                          s1) exprType
                 , apply (s5 `composeSubst` s4 `composeSubst`
                          s3 `composeSubst` s2 `composeSubst`
                          s1) resultType')

addScheme :: TypeEnv -> (String, Scheme) -> TI TypeEnv
addScheme env (name, scheme) =
  if Map.member name (varsMap env) then
    throwError $ "redefinition of " ++ name ++ " shadows an existing definition"
  else return $
    TypeEnv { varsMap = Map.insert name scheme (varsMap env)
            , variantsMap = variantsMap env
            , typesMap = typesMap env }

addVariant :: TypeEnv -> (String, NewConstructor) -> TypeEnv
addVariant env (name, scheme) =
  TypeEnv { varsMap = varsMap env
          , variantsMap = Map.insert name scheme (variantsMap env)
          , typesMap = typesMap env }

addRegisteredType :: TypeEnv -> RegisteredType -> TI TypeEnv
addRegisteredType env (RegisteredType name num) =
  if Map.member name (typesMap env) then
    throwError $ "redefinition of type " ++ name
  else return $
    TypeEnv { varsMap = varsMap env
            , variantsMap = variantsMap env
            , typesMap = Map.insert name num (typesMap env) }

casePartToType :: TypeEnv -> Pattern -> Subst ->
                  TI (Type, Map.Map String Type, Subst)
casePartToType _ PAny _ = do
  var <- newTyVar ()
  return (var, Map.empty, nullSubst)

casePartToType _ (PValue (Ident n)) _ = do
  var <- newTyVar ()
  return (var, Map.singleton n var, nullSubst)

casePartToType env (PVariant (Ident ident) paramPatterns) subst1 = do
  (cType, cParamTypes) <- getConstType
  when (length cParamTypes /= length paramPatterns)
      (throwError $ "parameter number mismatch in case expression " ++
                   "for constructor " ++ ident)
  (paramTypes, vars, subst3) <-
    foldM goParam ([], Map.empty, subst1) paramPatterns

  subst4 <- foldM uniParam subst3 (zip cParamTypes paramTypes)

  return (apply (subst4 `composeSubst` subst3) cType, vars
         , subst4 `composeSubst` subst3 `composeSubst`
           subst1)
  where
    goParam (prevTypes, prevVars, subst2) patt = do
            (patternType, patternVars, subst3) <- casePartToType env patt subst2
            return ( prevTypes ++ [patternType]
                   , prevVars `Map.union` patternVars
                      -- todo: errors on conflicts in union
                   , subst3)
    uniParam :: Subst -> (Type, Type) -> TI Subst
    uniParam subst2 (type1, type2) = do
      subst3 <- mgu (apply subst2 type1) (apply subst2 type2)
      return $ subst3 `composeSubst` subst2

    getConstType :: TI (Type, [Type])
    getConstType =
      maybe
        (throwError $ "constructor " ++ ident ++ " undefined")
        (\x -> x ())
        (Map.lookup ident (variantsMap env))

data RegisteredType = RegisteredType String Int;

-- returns all defined types, does not check duplicates
findTypes :: [Decl] -> [RegisteredType]
findTypes decls = do catMaybes (map extract decls)
  where
    extract (DData (TDecl (Ident name) args) _) =
      Just $ RegisteredType name (length args)
    extract _ = Nothing

tiDecls :: TypeEnv -> [Decl] -> TI (Subst, TypeEnv)
tiDecls env decls =
  do let types = findTypes decls
     env' <- foldM addRegisteredType env types
     (s, e, undefList) <- foldM go (nullSubst, env', []) decls
     if not (null undefList) then
       throwError $ "declared type, but undefined " ++ intercalate ", " undefList
     else
       return (s, e)
  where
    go :: (Subst, TypeEnv, [String]) -> Decl -> TI (Subst, TypeEnv, [String])
    go (s1, env', undef) decl = do
      (s2, env'', undef') <- tiDecl env' decl undef
      return (s1 `composeSubst` s2, env'', undef')

-- type inference on a declaration
-- returns modified type env and substitution that will be applied
-- to the let expression containing the declaration if any
-- [String] is a list of identifiers, for which a type was declared, but
-- not are undefined
tiDecl :: TypeEnv -> Decl -> [String] -> TI (Subst, TypeEnv, [String])
tiDecl env (DValue (Ident x) e1) undefList =
  enhanceErrorStack ("delcaration of " ++ x) $
    do  (s1, t1) <- ti env e1
        let t' = generalize (apply s1 env) t1
        if elem x undefList then
          -- todo: check if the type matches type declaration
          return (s1, apply s1 env, delete x undefList)
        else do
          env' <- addScheme env (x, t')
          return (s1, apply s1 env', undefList)

tiDecl env (DType (Ident name) typeRef) undefList =
  enhanceErrorStack ("declaration of type of " ++ name) $ do
    let dupVars = typeVariables typeRef
    let uniqVars = nub dupVars
    freeVars <- mapM (newTyVar . const ()) uniqVars
    let freeVarsMap = Map.fromList (zip uniqVars freeVars)
    t <- transTypeRef env freeVarsMap typeRef
    let s = generalize env t
    env' <- addScheme env (name, s)
    return (nullSubst, env', name:undefList)
  where
    typeVariables :: TypeRef -> [String]
    typeVariables (TRVariant _ refs) =
      concatMap typeVariables refs
    typeVariables (TRValue (Ident iName)) = [iName]
    typeVariables (TRFunc ref1 ref2) =
      typeVariables ref1 ++ typeVariables ref2


tiDecl env (DData (TDecl (Ident name) args) variants) undefList =
  enhanceErrorStack ("declaration of type " ++ name) $
    do
      freeVars <- mapM (newTyVar . const ()) args
      let freeVarsMap = Map.fromList (zip argNames freeVars)
      (s, t) <- foldM (go freeVars freeVarsMap) (nullSubst, env) variants
      return (s, t, undefList)
  where
    argNames = map unIdent args
    go :: [Type] -> Map.Map String Type -> (Subst, TypeEnv) ->
          Variant -> TI (Subst, TypeEnv)
    go freeVars freeVarsMap (s1, env') variant = do
      env'' <- declVariant freeVars freeVarsMap env' name argNames variant
      return (s1, env'')

    unIdent (Ident identName) = identName


declVariant :: [Type] -> Map.Map String Type -> TypeEnv ->
               String -> [String] -> Variant -> TI TypeEnv
declVariant freeVars _ env typeName paramNames
            variant@(Var (Ident varName) _) =
    do
        (baseType, paramTypes) <- buildConstType env typeName paramNames variant freeVars
        let t = foldl (flip TFun) baseType (reverse paramTypes)
        let t' = generalize env t
        env' <- addScheme env (varName, t')
        let env'' = addVariant env' (varName, typeFunc)
        return env''
  where
    typeFunc :: NewConstructor
    typeFunc _ = do
      newFreeVars <- mapM (const (newTyVar ())) freeVars
      buildConstType env typeName paramNames variant newFreeVars

buildConstType :: TypeEnv -> String -> [String] -> Variant -> [Type] -> TI (Type, [Type])
buildConstType env typeName typeParams (Var (Ident _) typeRefs) freeVars =
  let freeVarsMap = Map.fromList (zip typeParams freeVars) in do
    paramTypes <- mapM (transTypeRef env freeVarsMap) typeRefs
    return (TVariant typeName freeVars, paramTypes)


transTypeRef :: TypeEnv -> Map.Map String Type -> TypeRef -> TI Type
transTypeRef env freeVarsMap (TRVariant (Ident ident) typeRefs)
  | ident == intVerboseName && not (null typeRefs) =
      throwError "type Integer does not take parameters"
  | ident == intVerboseName = return TInt
  | otherwise =
      case Map.lookup ident (typesMap env) of
        Nothing -> throwError $ "reference to undefined type " ++ ident
        Just a | a /= length typeRefs ->
          throwError $ "expected " ++ show a ++ " parameters to " ++ ident ++
              ", got " ++ show (length typeRefs)
        Just _ -> do
          params <- mapM (transTypeRef env freeVarsMap) typeRefs
          return $ TVariant ident params

transTypeRef _ freeVarsMap (TRValue (Ident ident)) = return $ freeVarsMap Map.! ident
transTypeRef env freeVarsMap (TRFunc typeRef1 typeRef2) =
  do
    type1 <- transTypeRef env freeVarsMap typeRef1
    type2 <- transTypeRef env freeVarsMap typeRef2
    return $ TFun type1 type2

-- running the inference algorithm
typeInference :: TypeEnv -> Program -> Program -> TI Type
typeInference env1 (Program builtinDecls) (Program decls) =
  do
    (s1, env2) <- enhanceErrorStack "builtins" (tiDecls env1 builtinDecls)
    let env3 = apply s1 env2
    (s2, t) <- ti env3 e
    return (apply (s2 `composeSubst` s1) t)

  where
    e = ELet decls (EVar (Ident "main"))

enhanceErrorStack :: String -> TI a -> TI a
enhanceErrorStack item errorExpr =
  do
    errorExpr
  `catchError`
  \e -> throwError $ e ++ "\n in " ++ item


testWithBuiltins :: Program -> Program -> Either String Type
testWithBuiltins builtinProgram program =
    fst (runTI (typeInference builtinEnv builtinProgram program))
  where
        builtinEnv = TypeEnv { varsMap = builtins
                             , variantsMap = Map.empty
                             , typesMap = Map.singleton intVerboseName 0 }
        builtins = Map.fromList
                    [ ("+", intIntInt)
                    , ("-", intIntInt)
                    , ("*", intIntInt)
                    , ("`div`", intIntInt)
                    , ("==", intIntBool)
                    , ("/=", intIntBool)
                    , ("<=", intIntBool)
                    , ("<", intIntBool)
                    , (">=", intIntBool)
                    , (">", intIntBool)
                    ]
        intIntInt = Scheme [] $ TFun TInt (TFun TInt TInt)
        intIntBool = Scheme [] $ TFun TInt (TFun TInt (TVariant "Bool" []))

-- printing the type
instance Show Type where
    showsPrec _ x = shows (prType x)

prType             ::  Type -> PP.Doc
prType (TVar n)    =   PP.text n
prType TInt        =   PP.text intVerboseName
prType (TFun t s)  =   prParenType t PP.<+> PP.text "->" PP.<+> prType s
prType (TVariant n params) =  PP.text n PP.<+>
                                PP.hsep (map prParenType params)

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      TFun _ _  -> PP.parens (prType t)
                      _         -> prType t

instance Show Lit where
    showsPrec _ x = shows (prLit x)

prLit            ::  Lit -> PP.Doc
prLit (LInt i)   =   PP.integer i

instance Show Scheme where
    showsPrec _ x = shows (prScheme x)

prScheme                  ::  Scheme -> PP.Doc
prScheme (Scheme vars t)  =   PP.text "All" PP.<+>
                              PP.hcat
                                (PP.punctuate PP.comma (map PP.text vars))
                              PP.<> PP.text "." PP.<+> prType t

