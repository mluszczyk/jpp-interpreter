module SkelGrammar where

-- Haskell module generated by the BNF converter

import AbsGrammar
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EApp exp1 exp2 -> failure x
  EIf exp1 exp2 exp3 -> failure x
  ELet ident exp1 exp2 -> failure x
  EWhere exp decls -> failure x
  ELambda ident exp -> failure x
  EAdd exp1 exp2 -> failure x
  ESub exp1 exp2 -> failure x
  EMul exp1 exp2 -> failure x
  EDiv exp1 exp2 -> failure x
  EInt integer -> failure x
  EVar ident -> failure x
transTypeDecl :: TypeDecl -> Result
transTypeDecl x = case x of
  TDecl ident idents -> failure x
transTypeRef :: TypeRef -> Result
transTypeRef x = case x of
  TRef ident typerefs -> failure x
transVariant :: Variant -> Result
transVariant x = case x of
  Var ident typerefs -> failure x
transDecl :: Decl -> Result
transDecl x = case x of
  DConst ident idents exp -> failure x
  DData typedecl variants -> failure x

