module SimpleGrammar where

newtype Program = Program [Decl] deriving (Eq, Ord, Show, Read)

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)

data Exp
    = ELet [Decl] Exp
    | ECase Exp [CasePart]
    | ELambda Ident Exp
    | EApp Exp Exp
    | EInt Integer
    | EVar Ident
  deriving (Eq, Ord, Show, Read)

data Decl
    = DValue Ident [Ident] Exp
    | DType Ident TypeRef
    | DData TypeDecl [Variant]
  deriving (Eq, Ord, Show, Read)

data CasePart = CaseP Pattern Exp
  deriving (Eq, Ord, Show, Read)

data Pattern
    = PVariant Ident [Pattern] | PValue Ident | PAny
  deriving (Eq, Ord, Show, Read)

data TypeDecl = TDecl Ident [Ident]
  deriving (Eq, Ord, Show, Read)

data TypeRef
    = TRVariant Ident [TypeRef]
    | TRFunc TypeRef TypeRef
    | TRValue Ident
    | TRSimpleVariant Ident
  deriving (Eq, Ord, Show, Read)

data Variant = Var Ident [TypeRef]
  deriving (Eq, Ord, Show, Read)

