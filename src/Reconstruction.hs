{-
Algorithm W according to Martin Grabmuller paper.
Available as JPP course material.
-}

module Reconstruction where 

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Text.PrettyPrint as PP

import SimpleGrammar

data Lit     =  LInt Integer
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

    apply s (TVar n)      =  case Map.lookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply _ TInt             =  TInt
    apply s (TVariant name types) = (TVariant name (map (apply s) types))

instance Types Scheme where
    ftv (Scheme vars t)      =  (ftv t) `Set.difference` (Set.fromList vars)

    apply s (Scheme vars t)  =  Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv l    =  foldr Set.union Set.empty (map ftv l)

type Subst = Map.Map String Type

nullSubst  ::  Subst
nullSubst  =   Map.empty

composeSubst         :: Subst -> Subst -> Subst
composeSubst s1 s2   = (Map.map (apply s1) s2) `Map.union` s1

-- like env in the interpreter, but stores types (schemes to be precise)
-- rather than values
data TypeEnv = TypeEnv { varsMap :: Map.Map String Scheme
                       , variantsMap :: 
                          Map.Map String (() -> TI (Type, [Type]))
                       }

-- remove variable from type env
remove                    ::  TypeEnv -> String -> TypeEnv
remove envStruct var  =
      TypeEnv {varsMap = Map.delete var (varsMap envStruct)
              , variantsMap = variantsMap envStruct}

instance Types TypeEnv where
    ftv env      =  ftv (Map.elems (varsMap env))
    apply s env  =  TypeEnv { varsMap = (Map.map (apply s) (varsMap env))
                            , variantsMap = variantsMap env }

-- converts a type to a scheme by fetching all variables not bound by env
generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

-- tiSupply - variable counter used to create new varaibles
data TIState = TIState { tiSupply :: Int }

-- state is for the variable counter
-- except is for unification errors
type TI a = ExceptT String (State TIState) a

runTI :: TI a -> (Either String a, TIState)
runTI t = 
    runState (runExceptT t) initTIState
  where initTIState = TIState{tiSupply = 0}

-- create type variable
newTyVar :: String -> TI Type
newTyVar prefix =
    do  s <- get
        put s{tiSupply = tiSupply s + 1}
        return (TVar  (prefix ++ show (tiSupply s)))

-- ?
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do  nvars <- mapM (\ _ -> newTyVar "a") vars
                                  let s = Map.fromList (zip vars nvars)
                                  return $ apply s t

-- unification
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
    do  tv <- newTyVar "a"
        let env' = remove env n
            env'' = TypeEnv
              { varsMap = (varsMap env') 
                    `Map.union` (Map.singleton n (Scheme [] tv))
              , variantsMap = variantsMap env'
              }
        (s1, t1) <- ti env'' e
        return (s1, TFun (apply s1 tv) t1)
ti env expr@(EApp e1 e2) =
    do  tv <- newTyVar "a"
        (s1, t1) <- ti env e1
        (s2, t2) <- ti (apply s1 env) e2
        s3 <- mgu (apply s2 t1) (TFun t2 tv)
        return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
    `catchError`
    \e -> throwError $ e ++ "\n in " ++ show expr

ti env (ELet decls e) =
    do  (s1, env1) <- tiDecls env decls
        (s2, t) <- ti (apply s1 env1) e
        return (s1 `composeSubst` s2, t)

ti env (ECase expr caseParts) = do
    (subst, exprType) <- ti env expr
    resultType <- newTyVar "a"
    (s, _, t) <- foldM go (subst, exprType, resultType) caseParts
    return (s, t)

  where 
    go (s1, exprType, resultType) (CaseP pattern result) = do
          (patternType, varMap, s2) <- casePartToType env pattern s1
          s3 <- mgu patternType (apply s2 exprType)
          let envMapUpdate = Map.map (Scheme []) varMap
          (s4, resultType') <- ti (apply (s3 `composeSubst` s2)
              (TypeEnv { varsMap = envMapUpdate `Map.union` (varsMap env)
                       , variantsMap = variantsMap env
                       })) result
          s5 <- mgu (apply (
            s4 `composeSubst` s3 `composeSubst` s2
            ) resultType) resultType'
          return (s5 `composeSubst` s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
                 , apply (s5 `composeSubst` s4 `composeSubst` s3 `composeSubst` s2) exprType
                 , apply (s5 `composeSubst` s4 `composeSubst` s3 `composeSubst` s2) resultType')

casePartToType :: TypeEnv -> Pattern -> Subst -> 
                  TI (Type, Map.Map String Type, Subst)
casePartToType _ (PAny) _ = do
  var <- newTyVar "a"
  return (var, Map.empty, nullSubst)

casePartToType _ (PValue (Ident n)) _ = do
  var <- newTyVar "a"
  return (var, Map.singleton n var, nullSubst)

casePartToType env (PVariant (Ident ident) paramPatterns) subst1 = do
  (cType, cParamTypes) <- getConstType env ident
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
    goParam (prevTypes, prevVars, subst2) pattern = do
            (patternType, patternVars, subst3) <- casePartToType env pattern subst2
            return ( prevTypes ++ [patternType]
                   , prevVars `Map.union` patternVars 
                      -- todo: errors on conflicts in union
                   , subst3)
    uniParam :: Subst -> (Type, Type) -> TI Subst
    uniParam subst2 (type1, type2) =
      mgu (apply subst2 type1) (apply subst2 type2)

getConstType :: TypeEnv -> String -> TI (Type, [Type])
getConstType env consName =
  maybe 
    (throwError $ "constructor " ++ consName ++ " undefined")
    (\x -> do x ())
    (Map.lookup consName (variantsMap env))


tiDecls :: TypeEnv -> [Decl] -> TI (Subst, TypeEnv)
tiDecls env decls =
  foldM go (nullSubst, env) decls
    where
      go :: (Subst, TypeEnv) -> Decl -> TI (Subst, TypeEnv)
      go (s1, env') decl = do
        (s2, env'') <- tiDecl env' decl
        return (s1 `composeSubst` s2, env'')

tiDecl :: TypeEnv -> Decl -> TI (Subst, TypeEnv)
tiDecl env (DValue (Ident x) e1) =
    do  (s1, t1) <- ti env e1
        let env' = remove env x
            t' = generalize (apply s1 env) t1
            env'' = TypeEnv { varsMap = Map.insert x t' (varsMap env')
                            , variantsMap = variantsMap env'
                            }
        return (s1, env'')


tiDecl env (DData (TDecl (Ident name) args) variants) =
  do
    freeVars <- mapM (newTyVar) (map (const "a") args)
    let freeVarsMap = Map.fromList (zip argNames freeVars)
    foldM (go freeVars freeVarsMap) (nullSubst, env) variants 
  where
    argNames = map unIdent args
    go :: [Type] -> (Map.Map String Type) -> (Subst, TypeEnv) ->
          Variant -> TI (Subst, TypeEnv)
    go freeVars freeVarsMap (s1, env') variant = do
      (s2, env'') <- declVariant freeVars freeVarsMap env' name argNames variant
      return (s1 `composeSubst` s2, env'')

    unIdent (Ident identName) = identName


declVariant :: [Type] -> (Map.Map String Type) -> TypeEnv ->
               String -> [String] -> Variant -> TI (Subst, TypeEnv)
declVariant freeVars _ env typeName paramNames 
            variant@(Var (Ident varName) typeRefs) =
    do
        (baseType, paramTypes) <- buildConstType typeName paramNames variant freeVars
        let t = foldl (flip TFun) baseType (reverse paramTypes)
        let
            env' = remove env varName
            t' = generalize env t
            env'' = TypeEnv { varsMap = Map.insert varName t' (varsMap env')
                            , variantsMap =
                              Map.insert varName typeFunc (variantsMap env')
                            }
        return (nullSubst, env'')
  where
    typeFunc :: () -> TI (Type, [Type])
    typeFunc _ = do
      newFreeVars <- mapM (const (newTyVar "a")) freeVars
      buildConstType typeName paramNames variant newFreeVars

buildConstType :: String -> [String] -> Variant -> [Type] -> TI (Type, [Type])
buildConstType typeName typeParams (Var (Ident varName) typeRefs) freeVars =
  let freeVarsMap = Map.fromList (zip typeParams freeVars) in do
    paramTypes <- mapM (transTypeRef freeVarsMap) typeRefs
    return (TVariant typeName freeVars, paramTypes)
  

transTypeRef :: (Map.Map String Type) -> TypeRef -> TI Type
transTypeRef freeVarsMap (TRVariant (Ident ident) typeRefs) =
  do
    params <- mapM (transTypeRef freeVarsMap) typeRefs
    return $ TVariant ident params -- todo: check existance and num of parameters
transTypeRef freeVarsMap (TRValue (Ident ident)) = return $ freeVarsMap Map.! ident
transTypeRef freeVarsMap (TRFunc typeRef1 typeRef2) =
  do
    type1 <- transTypeRef freeVarsMap typeRef1
    type2 <- transTypeRef freeVarsMap typeRef2
    return $ TFun type1 type2

-- running the inference algorithm
typeInference :: TypeEnv -> Exp -> TI Type
typeInference env e =
    do  (s, t) <- ti env e
        return (apply s t)

testExp :: Exp -> IO ()
testExp e =
    case fst (runTI (typeInference builtinEnv e)) of
        Left err  ->  putStrLn $ show e ++ "\n " ++ err ++ "\n"
        Right t   ->  putStrLn $ "Reconstruction: main :: " ++ show t ++ "\n"
  where 
        builtinEnv = TypeEnv {varsMap = builtins, variantsMap = Map.empty}
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

testProgram :: Program -> IO ()
testProgram (Program declsList) =
    testExp (ELet declsList (EVar (Ident "main")))


test :: Program -> IO ()
test = testProgram

-- printing the type
instance Show Type where
    showsPrec _ x = shows (prType x)

prType             ::  Type -> PP.Doc
prType (TVar n)    =   PP.text n
prType TInt        =   PP.text "Int"
prType (TFun t s)  =   prParenType t PP.<+> PP.text "->" PP.<+> prType s
prType (TVariant n params) =  PP.text n PP.<+>
                                PP.hsep (map prType params)

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

