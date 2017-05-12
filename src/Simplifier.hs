module Simplifier where

import qualified AbsGrammar as AG
import qualified SimpleGrammar as SG

simplify :: AG.Program -> SG.Program
simplify (AG.Program decls) =
  SG.Program $ simplifyDeclBlock decls

simplifyValueIdent :: AG.ValueIdent -> SG.Ident
simplifyValueIdent (AG.ValueIdent name) = SG.Ident name

simplifyTypeIdent :: AG.TypeIdent -> SG.Ident
simplifyTypeIdent (AG.TypeIdent name) = SG.Ident name

simplifyDeclBlock :: [AG.Decl] -> [SG.Decl]
simplifyDeclBlock = map simplifyDecl

simplifyDecl :: AG.Decl -> SG.Decl
simplifyDecl (AG.DValue name args expr) =
  SG.DValue (simplifyValueIdent name) (simplifyExp (desugarFunc args expr))
  where
    desugarFunc :: [AG.ValueIdent] -> AG.Exp -> AG.Exp
    desugarFunc argsIdents expr' =
      Prelude.foldr AG.ELambda expr' argsIdents

simplifyDecl (AG.DType name typeRef) =
  SG.DType (simplifyValueIdent name) (simplifyTypeRef typeRef)
simplifyDecl (AG.DData typeDecl variants) =
  SG.DData (simplifyTypeDecl typeDecl) (map simplifyVariants variants)

simplifyTypeDecl :: AG.TypeDecl -> SG.TypeDecl
simplifyTypeDecl (AG.TDecl typeIdent  valueIdents) =
  SG.TDecl (simplifyTypeIdent typeIdent) (map simplifyValueIdent valueIdents)

simplifyVariants :: AG.Variant -> SG.Variant
simplifyVariants (AG.Var typeIdent typeRefs) =
  SG.Var (simplifyTypeIdent typeIdent) (map simplifyTypeRef typeRefs)
simplifyVariants (AG.SimpleVar typeIdent) =
  SG.Var (simplifyTypeIdent typeIdent) []

simplifyTypeRef :: AG.TypeRef -> SG.TypeRef
simplifyTypeRef (AG.TRValue ident) = SG.TRValue (simplifyValueIdent ident)
simplifyTypeRef (AG.TRFunc typeRef1 typeRef2) =
  SG.TRFunc (simplifyTypeRef typeRef1) (simplifyTypeRef typeRef2)
simplifyTypeRef (AG.TRVariant ident typeRefs) =
  SG.TRVariant (simplifyTypeIdent ident) (map simplifyTypeRef typeRefs)
simplifyTypeRef (AG.TRSimpleVariant ident) =
  SG.TRVariant (simplifyTypeIdent ident) []

unaryOperation :: String -> AG.Exp -> SG.Exp
unaryOperation s e1 =
  SG.EApp (SG.EVar (SG.Ident s)) (simplifyExp e1)

binaryOperation :: String -> AG.Exp -> AG.Exp -> SG.Exp
binaryOperation s e1 e2 =
  SG.EApp (unaryOperation s e1) (simplifyExp e2)

ternaryOperation :: String -> AG.Exp -> AG.Exp -> AG.Exp -> SG.Exp
ternaryOperation s e1 e2 e3 =
  SG.EApp (binaryOperation s e1 e2) (simplifyExp e3)

simplifyExp :: AG.Exp -> SG.Exp
simplifyExp expr = case expr of
  AG.EAdd expr1 expr2 -> binaryOperation "+" expr1 expr2
  AG.ESub expr1 expr2 -> binaryOperation "-" expr1 expr2
  AG.EMul expr1 expr2 -> binaryOperation "*" expr1 expr2
  AG.EDiv expr1 expr2 -> binaryOperation "`div`" expr1 expr2
  AG.ELT expr1 expr2 -> binaryOperation "<" expr1 expr2
  AG.ELTE expr1 expr2 -> binaryOperation "<=" expr1 expr2
  AG.EGT expr1 expr2 -> binaryOperation ">" expr1 expr2
  AG.EGTE expr1 expr2 -> binaryOperation ">=" expr1 expr2
  AG.EEq expr1 expr2 -> binaryOperation "==" expr1 expr2
  AG.ENEq expr1 expr2 -> binaryOperation "/=" expr1 expr2
  AG.EAnd expr1 expr2 -> binaryOperation "and_" expr1 expr2
  AG.EOr expr1 expr2 -> binaryOperation "or_" expr1 expr2
  AG.EIf expr1 expr2 expr3 -> ternaryOperation "if_" expr1 expr2 expr3

  AG.EInt integer -> SG.EInt integer
  AG.ELet decls expr' -> SG.ELet (simplifyDeclBlock decls) (simplifyExp expr')
  AG.EVarValue name -> SG.EVar (simplifyValueIdent name)
  AG.EVarType name -> SG.EVar (simplifyTypeIdent name)
  AG.ELambda value expr' ->
    SG.ELambda (simplifyValueIdent value) (simplifyExp expr')
  AG.EApp expr1 expr2 -> SG.EApp (simplifyExp expr1) (simplifyExp expr2)
  AG.ECase expr' caseParts -> SG.ECase (simplifyExp expr') (
    map simplifyCasePart caseParts)

simplifyCasePart :: AG.CasePart -> SG.CasePart
simplifyCasePart (AG.CaseP pat expr) =
  SG.CaseP (simplifyPattern pat) (simplifyExp expr)

simplifyPattern :: AG.Pattern -> SG.Pattern
simplifyPattern (AG.PVariant typeIdent patterns) =
  SG.PVariant (simplifyTypeIdent typeIdent) (map simplifyPattern patterns) 
simplifyPattern (AG.PValue valueIdent) =
  SG.PValue (simplifyValueIdent valueIdent)
simplifyPattern AG.PAny = SG.PAny
