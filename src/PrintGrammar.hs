{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintGrammar where

-- pretty-printer generated by the BNF converter

import AbsGrammar
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

instance Print TypeIdent where
  prt _ (TypeIdent i) = doc (showString ( i))


instance Print ValueIdent where
  prt _ (ValueIdent i) = doc (showString ( i))
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])


instance Print Program where
  prt i e = case e of
    Program decls -> prPrec i 0 (concatD [prt 0 decls])

instance Print Exp where
  prt i e = case e of
    ELet decls exp -> prPrec i 0 (concatD [doc (showString "let"), doc (showString "{"), prt 0 decls, doc (showString "}"), doc (showString "in"), prt 0 exp])
    ECase exp caseparts -> prPrec i 0 (concatD [doc (showString "case"), prt 0 exp, doc (showString "of"), doc (showString "{"), prt 0 caseparts, doc (showString "}")])
    ELambda valueident exp -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 valueident, doc (showString "->"), prt 0 exp])
    EIf exp1 exp2 exp3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 exp1, doc (showString "then"), prt 0 exp2, doc (showString "else"), prt 0 exp3])
    EAnd exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "&&"), prt 2 exp2])
    EOr exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "||"), prt 2 exp2])
    ELT exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "<"), prt 3 exp2])
    ELTE exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "<="), prt 3 exp2])
    EGT exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString ">"), prt 3 exp2])
    EGTE exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString ">="), prt 3 exp2])
    EEq exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "=="), prt 3 exp2])
    ENEq exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "/="), prt 3 exp2])
    EAdd exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "+"), prt 4 exp2])
    ESub exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "-"), prt 4 exp2])
    EMul exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "*"), prt 5 exp2])
    EDiv exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "`div`"), prt 5 exp2])
    EApp exp1 exp2 -> prPrec i 10 (concatD [prt 10 exp1, prt 11 exp2])
    EInt n -> prPrec i 11 (concatD [prt 0 n])
    EVarValue valueident -> prPrec i 11 (concatD [prt 0 valueident])
    EVarType typeident -> prPrec i 11 (concatD [prt 0 typeident])

instance Print Decl where
  prt i e = case e of
    DValue valueident valueidents exp -> prPrec i 0 (concatD [prt 0 valueident, prt 0 valueidents, doc (showString "="), prt 0 exp])
    DValueWhere valueident valueidents exp decls -> prPrec i 0 (concatD [prt 0 valueident, prt 0 valueidents, doc (showString "="), prt 0 exp, doc (showString "where"), doc (showString "{"), prt 0 decls, doc (showString "}")])
    DType valueident typeref -> prPrec i 0 (concatD [prt 0 valueident, doc (showString "::"), prt 0 typeref])
    DData typedecl variants -> prPrec i 0 (concatD [doc (showString "data"), prt 0 typedecl, doc (showString "="), prt 0 variants])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print CasePart where
  prt i e = case e of
    CaseP pattern exp -> prPrec i 0 (concatD [prt 0 pattern, doc (showString "->"), prt 0 exp])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Pattern where
  prt i e = case e of
    PVariant typeident patterns -> prPrec i 0 (concatD [prt 0 typeident, prt 1 patterns])
    PSimpleVariant typeident -> prPrec i 1 (concatD [prt 0 typeident])
    PValue valueident -> prPrec i 1 (concatD [prt 0 valueident])
    PAny -> prPrec i 1 (concatD [doc (showString "_")])
  prtList 1 [x] = (concatD [prt 1 x])
  prtList 1 (x:xs) = (concatD [prt 1 x, prt 1 xs])
instance Print TypeDecl where
  prt i e = case e of
    TDecl typeident valueidents -> prPrec i 0 (concatD [prt 0 typeident, prt 0 valueidents])

instance Print TypeRef where
  prt i e = case e of
    TRVariant typeident typerefs -> prPrec i 0 (concatD [prt 0 typeident, prt 1 typerefs])
    TRFunc typeref1 typeref2 -> prPrec i 1 (concatD [prt 2 typeref1, doc (showString "->"), prt 1 typeref2])
    TRValue valueident -> prPrec i 2 (concatD [prt 0 valueident])
    TRSimpleVariant typeident -> prPrec i 2 (concatD [prt 0 typeident])
  prtList 1 [x] = (concatD [prt 1 x])
  prtList 1 (x:xs) = (concatD [prt 1 x, prt 1 xs])
instance Print Variant where
  prt i e = case e of
    Var typeident typerefs -> prPrec i 0 (concatD [prt 0 typeident, prt 1 typerefs])
    SimpleVar typeident -> prPrec i 0 (concatD [prt 0 typeident])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "|"), prt 0 xs])

