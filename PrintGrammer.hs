-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintGrammer.

module PrintGrammer where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsGrammer

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsGrammer.Ident where
  prt _ (AbsGrammer.Ident i) = doc $ showString i
instance Print AbsGrammer.ArOp where
  prt i = \case
    AbsGrammer.OpAdd -> prPrec i 0 (concatD [doc (showString "+")])
    AbsGrammer.OpSub -> prPrec i 0 (concatD [doc (showString "-")])
    AbsGrammer.OpMul -> prPrec i 0 (concatD [doc (showString "*")])
    AbsGrammer.OpDiv -> prPrec i 0 (concatD [doc (showString "//")])

instance Print AbsGrammer.CompOp where
  prt i = \case
    AbsGrammer.OpEq -> prPrec i 0 (concatD [doc (showString "==")])
    AbsGrammer.OpNeq -> prPrec i 0 (concatD [doc (showString "!=")])
    AbsGrammer.OpGr -> prPrec i 0 (concatD [doc (showString ">")])
    AbsGrammer.OpGeq -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsGrammer.OpLe -> prPrec i 0 (concatD [doc (showString "<")])
    AbsGrammer.OpLeq -> prPrec i 0 (concatD [doc (showString "<=")])

instance Print AbsGrammer.BoolOp where
  prt i = \case
    AbsGrammer.OpAnd -> prPrec i 0 (concatD [doc (showString "and")])
    AbsGrammer.OpOr -> prPrec i 0 (concatD [doc (showString "or")])

instance Print AbsGrammer.Expr where
  prt i = \case
    AbsGrammer.ELitInt n -> prPrec i 0 (concatD [prt 0 n])
    AbsGrammer.ELitTrue -> prPrec i 0 (concatD [doc (showString "true")])
    AbsGrammer.ELitFalse -> prPrec i 0 (concatD [doc (showString "false")])
    AbsGrammer.EList exprs -> prPrec i 0 (concatD [doc (showString "["), prt 0 exprs, doc (showString "]")])
    AbsGrammer.EListHead expr -> prPrec i 0 (concatD [doc (showString "head"), prt 0 expr])
    AbsGrammer.EListTail expr -> prPrec i 0 (concatD [doc (showString "tail"), prt 0 expr])
    AbsGrammer.EListEmpty expr -> prPrec i 0 (concatD [doc (showString "empty"), prt 0 expr])
    AbsGrammer.EArOp arop expr1 expr2 -> prPrec i 0 (concatD [prt 0 arop, prt 0 expr1, prt 0 expr2])
    AbsGrammer.ECompOp compop expr1 expr2 -> prPrec i 0 (concatD [prt 0 compop, prt 0 expr1, prt 0 expr2])
    AbsGrammer.EBoolOp boolop expr1 expr2 -> prPrec i 0 (concatD [prt 0 boolop, prt 0 expr1, prt 0 expr2])
    AbsGrammer.EAnonFun id_ ids expr -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 id_, prt 0 ids, doc (showString "->"), prt 0 expr])
    AbsGrammer.EApp id_ exprs -> prPrec i 0 (concatD [prt 0 id_, doc (showString "args"), prt 0 exprs, doc (showString "endargs")])
    AbsGrammer.EIf expr1 expr2 expr3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expr1, doc (showString "then"), prt 0 expr2, doc (showString "else"), prt 0 expr3, doc (showString "endif")])
    AbsGrammer.EVar id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsGrammer.ELet id_ ids expr1 expr2 -> prPrec i 0 (concatD [doc (showString "let"), prt 0 id_, prt 0 ids, doc (showString "="), prt 0 expr1, doc (showString "in"), prt 0 expr2, doc (showString "endlet")])

instance Print [AbsGrammer.Expr] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsGrammer.Ident] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]