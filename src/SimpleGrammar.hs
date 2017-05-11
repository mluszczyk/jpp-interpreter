module SimpleGrammar where

data Program = Program [Decl] deriving (Eq, Ord, Show, Read)

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
newtype TypeIdent = TypeIdent String deriving (Eq, Ord, Show, Read)
newtype ValueIdent = ValueIdent String
  deriving (Eq, Ord, Show, Read)

data Exp
    = ELet [Decl] Exp
    | ECase Exp [CasePart]
    | ELambda ValueIdent Exp
    | EApp Exp Exp
    | EInt Integer
    | EVarValue ValueIdent
    | EVarType TypeIdent
  deriving (Eq, Ord, Show, Read)

data Decl
    = DValue ValueIdent [ValueIdent] Exp
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

data Variant = Var TypeIdent [TypeRef]
  deriving (Eq, Ord, Show, Read)

