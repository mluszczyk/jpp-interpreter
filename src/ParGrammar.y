-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGrammar where
import AbsGrammar
import LexGrammar
import ErrM

}

%name pExp Exp
%name pExp1 Exp1
%name pExp2 Exp2
%name pCasePart CasePart
%name pListCasePart ListCasePart
%name pPattern Pattern
%name pListPattern ListPattern
%name pTypeDecl TypeDecl
%name pTypeRef TypeRef
%name pListTypeRef ListTypeRef
%name pVariant Variant
%name pDecl Decl
%name pListIdent ListIdent
%name pListDecl ListDecl
%name pListVariant ListVariant
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  ' ' { PT _ (TS _ 1) }
  '(' { PT _ (TS _ 2) }
  ')' { PT _ (TS _ 3) }
  '*' { PT _ (TS _ 4) }
  '+' { PT _ (TS _ 5) }
  '-' { PT _ (TS _ 6) }
  '->' { PT _ (TS _ 7) }
  '/' { PT _ (TS _ 8) }
  ';' { PT _ (TS _ 9) }
  '=' { PT _ (TS _ 10) }
  '\\' { PT _ (TS _ 11) }
  '_' { PT _ (TS _ 12) }
  'case' { PT _ (TS _ 13) }
  'data' { PT _ (TS _ 14) }
  'else' { PT _ (TS _ 15) }
  'if' { PT _ (TS _ 16) }
  'in' { PT _ (TS _ 17) }
  'let' { PT _ (TS _ 18) }
  'of' { PT _ (TS _ 19) }
  'then' { PT _ (TS _ 20) }
  'where' { PT _ (TS _ 21) }
  '{' { PT _ (TS _ 22) }
  '|' { PT _ (TS _ 23) }
  '}' { PT _ (TS _ 24) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }

Exp :: { Exp }
Exp : Exp Exp1 { AbsGrammar.EApp $1 $2 }
    | 'if' Exp 'then' Exp 'else' Exp { AbsGrammar.EIf $2 $4 $6 }
    | 'let' Ident '=' Exp 'in' Exp { AbsGrammar.ELet $2 $4 $6 }
    | Exp 'where' '{' ListDecl '}' { AbsGrammar.EWhere $1 $4 }
    | 'case' Exp 'of' '{' ListCasePart '}' { AbsGrammar.ECase $2 $5 }
    | '\\' Ident '->' Exp { AbsGrammar.ELambda $2 $4 }
    | Exp '+' Exp1 { AbsGrammar.EAdd $1 $3 }
    | Exp '-' Exp1 { AbsGrammar.ESub $1 $3 }
    | Exp1 { $1 }
Exp1 :: { Exp }
Exp1 : Exp1 '*' Exp2 { AbsGrammar.EMul $1 $3 }
     | Exp1 '/' Exp2 { AbsGrammar.EDiv $1 $3 }
     | Exp2 { $1 }
Exp2 :: { Exp }
Exp2 : Integer { AbsGrammar.EInt $1 }
     | Ident { AbsGrammar.EVar $1 }
     | '(' Exp ')' { $2 }
CasePart :: { CasePart }
CasePart : Pattern '->' Exp { AbsGrammar.CaseP $1 $3 }
ListCasePart :: { [CasePart] }
ListCasePart : {- empty -} { [] }
             | CasePart { (:[]) $1 }
             | CasePart ';' ListCasePart { (:) $1 $3 }
Pattern :: { Pattern }
Pattern : '_' { AbsGrammar.PAny }
        | Ident ListPattern { AbsGrammar.PVariant $1 $2 }
        | '(' Pattern ')' { $2 }
ListPattern :: { [Pattern] }
ListPattern : {- empty -} { [] }
            | Pattern { (:[]) $1 }
            | Pattern ' ' ListPattern { (:) $1 $3 }
TypeDecl :: { TypeDecl }
TypeDecl : Ident ListIdent { AbsGrammar.TDecl $1 $2 }
TypeRef :: { TypeRef }
TypeRef : Ident ListTypeRef { AbsGrammar.TRef $1 $2 }
        | '(' TypeRef ')' { $2 }
ListTypeRef :: { [TypeRef] }
ListTypeRef : {- empty -} { [] }
            | TypeRef ListTypeRef { (:) $1 $2 }
            | {- empty -} { [] }
            | TypeRef ListTypeRef { (:) $1 $2 }
Variant :: { Variant }
Variant : Ident ListTypeRef { AbsGrammar.Var $1 $2 }
Decl :: { Decl }
Decl : Ident ListIdent '=' Exp { AbsGrammar.DConst $1 $2 $4 }
     | 'data' TypeDecl '=' ListVariant { AbsGrammar.DData $2 $4 }
ListIdent :: { [Ident] }
ListIdent : {- empty -} { [] } | Ident ListIdent { (:) $1 $2 }
ListDecl :: { [Decl] }
ListDecl : {- empty -} { [] }
         | Decl { (:[]) $1 }
         | Decl ';' ListDecl { (:) $1 $3 }
ListVariant :: { [Variant] }
ListVariant : {- empty -} { [] }
            | Variant { (:[]) $1 }
            | Variant '|' ListVariant { (:) $1 $3 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

