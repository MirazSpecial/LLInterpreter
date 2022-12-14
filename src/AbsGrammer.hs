-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language grammer.

module AbsGrammer where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data ArOp = OpAdd | OpSub | OpMul | OpDiv
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data CompOp = OpEq | OpNeq | OpGr | OpGeq | OpLe | OpLeq
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BoolOp = OpAnd | OpOr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EList [Expr]
    | EListHead Expr
    | EListTail Expr
    | EListEmpty Expr
    | EArOp ArOp Expr Expr
    | ECompOp CompOp Expr Expr
    | EBoolOp BoolOp Expr Expr
    | EAnonFun Ident [Ident] Expr
    | EApp Ident [Expr]
    | EIf Expr Expr Expr
    | EVar Ident
    | ELet Ident [Ident] Expr Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

