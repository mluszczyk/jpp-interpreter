module Simplifier where

import qualified AbsGrammar as AG
import qualified SimpleGrammar as SG

simplify :: AG.Program -> SG.Program
simplify (AG.Program decls) =
  SG.Program $ simplifyDeclBlock decls

simplifyValueIdent (AG.ValueIdent name) = SG.ValueIdent name
simplifyTypeIdent (AG.TypeIdent name) = SG.TypeIdent name

simplifyDeclBlock :: [AG.Decl] -> [SG.Decl]
simplifyDeclBlock decls =
  map simplifyDecl decls

simplifyDecl :: AG.Decl -> SG.Decl
simplifyDecl (AG.DValue name args exp) =
  SG.DValue (simplifyValueIdent name) 
    (map simplifyValueIdent args) (simplifyExp exp)
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

unaryOperation :: String -> AG.Exp -> SG.Exp
unaryOperation s e1 =
  SG.EApp (SG.EVarValue (SG.ValueIdent s)) (simplifyExp e1)

binaryOperation :: String -> AG.Exp -> AG.Exp -> SG.Exp
binaryOperation s e1 e2 =
  SG.EApp (unaryOperation s e1) (simplifyExp e2)

ternaryOperation :: String -> AG.Exp -> AG.Exp -> AG.Exp -> SG.Exp
ternaryOperation s e1 e2 e3 =
  SG.EApp (binaryOperation s e1 e2) (simplifyExp e3)

simplifyExp :: AG.Exp -> SG.Exp
simplifyExp exp = case exp of
  AG.EAdd exp1 exp2 -> binaryOperation "+" exp1 exp2
  AG.ESub exp1 exp2 -> binaryOperation "-" exp1 exp2
  AG.EMul exp1 exp2 -> binaryOperation "*" exp1 exp2
  AG.EDiv exp1 exp2 -> binaryOperation "`div`" exp1 exp2
  AG.ELT exp1 exp2 -> binaryOperation "<" exp1 exp2
  AG.ELTE exp1 exp2 -> binaryOperation "<=" exp1 exp2
  AG.EGT exp1 exp2 -> binaryOperation ">" exp1 exp2
  AG.EGTE exp1 exp2 -> binaryOperation ">=" exp1 exp2
  AG.EEq exp1 exp2 -> binaryOperation "==" exp1 exp2
  AG.ENEq exp1 exp2 -> binaryOperation "/=" exp1 exp2
  AG.EAnd exp1 exp2 -> binaryOperation "and_" exp1 exp2
  AG.EOr exp1 exp2 -> binaryOperation "or_" exp1 exp2
  AG.EIf exp1 exp2 exp3 -> ternaryOperation "if_" exp1 exp2 exp3

  AG.EInt integer -> SG.EInt integer
  AG.ELet decls exp -> SG.ELet (simplifyDeclBlock decls) (simplifyExp exp)
  AG.EVarValue name -> SG.EVarValue (simplifyValueIdent name)
  AG.EVarType name -> SG.EVarType (simplifyTypeIdent name)
  AG.ELambda value exp -> SG.ELambda (simplifyValueIdent value) (simplifyExp exp)
  AG.EApp exp1 exp2 -> SG.EApp (simplifyExp exp1) (simplifyExp exp2)
  AG.ECase exp caseParts -> SG.ECase (simplifyExp exp) (
    map simplifyCasePart caseParts)

simplifyCasePart :: AG.CasePart -> SG.CasePart
simplifyCasePart (AG.CaseP pattern exp) =
  SG.CaseP (simplifyPattern pattern) (simplifyExp exp)

simplifyPattern :: AG.Pattern -> SG.Pattern
simplifyPattern (AG.PVariant typeIdent patterns) =
  SG.PVariant (simplifyTypeIdent typeIdent) (map simplifyPattern patterns) 
simplifyPattern (AG.PValue valueIdent) =
  SG.PValue (simplifyValueIdent valueIdent)
simplifyPattern AG.PAny = SG.PAny
