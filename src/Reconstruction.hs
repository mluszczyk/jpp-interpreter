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
newtype TypeEnv = TypeEnv (Map.Map String Scheme)

-- remove variable from type env
remove                    ::  TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var  =  TypeEnv (Map.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env)      =  ftv (Map.elems env)
    apply s (TypeEnv env)  =  TypeEnv (Map.map (apply s) env)

-- converts a type to a scheme by fetching all variables not bound by env
generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

-- um, is this unused?
data TIEnv = TIEnv  {}

-- tiSupply - variable counter used to create new varaibles
data TIState = TIState { tiSupply :: Int }

-- IO is for putStrLn
-- reader is unused
-- state is for the variable counter
-- except is for unification errors
type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = 
    do (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
       return (res, st)
  where initTIEnv = TIEnv
        initTIState = TIState{tiSupply = 0}

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
ti (TypeEnv env) (EVar (Ident ident)) = 
    case Map.lookup ident env of
       Nothing     ->  throwError $ "unbound variable: " ++ ident
       Just sigma  ->  do  t <- instantiate sigma
                           return (nullSubst, t)

ti _ (EInt integer) = tiLit (LInt integer)

ti env (ELambda (Ident n) e) =
    do  tv <- newTyVar "a"
        let TypeEnv env' = remove env n
            env'' = TypeEnv (env' `Map.union` (Map.singleton n (Scheme [] tv)))
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

ti env (ECase expr caseParts) =
    case caseParts of
      [CaseP PAny expr'] -> ti env expr'
      [CaseP (PValue (Ident n)) expr'] ->
          ti env (EApp (ELambda (Ident n) expr') expr)
            
           

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
        let TypeEnv env' = remove env x
            t' = generalize (apply s1 env) t1
            env'' = TypeEnv (Map.insert x t' env')
        return (s1, env'')


tiDecl env (DData (TDecl (Ident name) args) variants) =
  do
    freeVars <- mapM (newTyVar) (map (const "a") args)
    let freeVarsMap = Map.fromList (zip (map unIdent args) freeVars)
    foldM (go freeVars freeVarsMap) (nullSubst, env) variants 
  where
    go :: [Type] -> (Map.Map String Type) -> (Subst, TypeEnv) ->
          Variant -> TI (Subst, TypeEnv)
    go freeVars freeVarsMap (s1, env') variant = do
      (s2, env'') <- declVariant freeVars freeVarsMap env' name args variant
      return (s1 `composeSubst` s2, env'')

    unIdent (Ident identName) = identName


declVariant :: [Type] -> (Map.Map String Type) -> TypeEnv ->
               String -> a -> Variant -> TI (Subst, TypeEnv)
declVariant freeVarsList freeVarsMap env typeName _ 
            (Var (Ident varName) typeRefs) =
    do
        t <- mType
        let
            TypeEnv env' = remove env varName
            t' = generalize env t
            env'' = TypeEnv (Map.insert varName t' env')
        return (nullSubst, env'')
    where base = TVariant typeName freeVarsList
          mType = foldM go base (reverse typeRefs)
          go :: Type -> TypeRef -> TI Type
          go rest ref =
            do
              cType <- transTypeRef freeVarsMap ref
              return $ TFun cType rest

transTypeRef :: (Map.Map String Type) -> TypeRef -> TI Type
transTypeRef freeVarsMap (TRVariant (Ident ident) typeRefs) =
  do
    params <- mapM (transTypeRef freeVarsMap) typeRefs
    return $ TVariant ident params
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
    do  (res, _) <- runTI (typeInference (TypeEnv builtins) e)
        case res of
          Left err  ->  putStrLn $ show e ++ "\n " ++ err ++ "\n"
          Right t   ->  putStrLn $ "Reconstruction: main :: " ++ show t ++ "\n"
  where builtins = Map.fromList
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

