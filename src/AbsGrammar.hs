

module AbsGrammar where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
newtype TypeIdent = TypeIdent String deriving (Eq, Ord, Show, Read)
newtype ValueIdent = ValueIdent String
  deriving (Eq, Ord, Show, Read)
data Program = Program [Decl]
  deriving (Eq, Ord, Show, Read)

data Exp
    = ELet [Decl] Exp
    | ECase Exp [CasePart]
    | ELambda ValueIdent Exp
    | EIf Exp Exp Exp
    | EAnd Exp Exp
    | EOr Exp Exp
    | ELT Exp Exp
    | ELTE Exp Exp
    | EGT Exp Exp
    | EGTE Exp Exp
    | EEq Exp Exp
    | ENEq Exp Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EApp Exp Exp
    | EInt Integer
    | EVarValue ValueIdent
    | EVarType TypeIdent
  deriving (Eq, Ord, Show, Read)

data Decl
    = DValue ValueIdent [ValueIdent] Exp
    | DValueWhere ValueIdent [ValueIdent] Exp [Decl]
    | DType ValueIdent TypeRef
    | DData TypeDecl [Variant]
  deriving (Eq, Ord, Show, Read)

data CasePart = CaseP Pattern Exp
  deriving (Eq, Ord, Show, Read)

data Pattern
    = PVariant TypeIdent [Pattern] | PValue ValueIdent | PAny
  deriving (Eq, Ord, Show, Read)

data TypeDecl = TDecl TypeIdent [ValueIdent]
  deriving (Eq, Ord, Show, Read)

data TypeRef
    = TRVariant TypeIdent [TypeRef]
    | TRFunc TypeRef TypeRef
    | TRValue ValueIdent
    | TRSimpleVariant TypeIdent
  deriving (Eq, Ord, Show, Read)

data Variant = Var TypeIdent [TypeRef] | SimpleVar TypeIdent
  deriving (Eq, Ord, Show, Read)

