

module AbsGrammar where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
newtype TypeIdent = TypeIdent String deriving (Eq, Ord, Show, Read)
newtype ValueIdent = ValueIdent String
  deriving (Eq, Ord, Show, Read)
data Exp
    = EApp Exp Exp
    | EIf Exp Exp Exp
    | ELet ValueIdent Exp Exp
    | EWhere Exp [Decl]
    | ECase Exp [CasePart]
    | ELambda ValueIdent Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EInt Integer
    | EVarValue ValueIdent
    | EVarType TypeIdent
  deriving (Eq, Ord, Show, Read)

data CasePart = CaseP Pattern Exp
  deriving (Eq, Ord, Show, Read)

data Pattern
    = PAny | PValue ValueIdent | PVariant TypeIdent [Pattern]
  deriving (Eq, Ord, Show, Read)

data TypeDecl = TDecl TypeIdent [ValueIdent]
  deriving (Eq, Ord, Show, Read)

data TypeRef = TRValue ValueIdent | TRVariant TypeIdent [TypeRef]
  deriving (Eq, Ord, Show, Read)

data Variant = Var TypeIdent [TypeRef]
  deriving (Eq, Ord, Show, Read)

data Decl
    = DValue ValueIdent [ValueIdent] Exp | DData TypeDecl [Variant]
  deriving (Eq, Ord, Show, Read)

