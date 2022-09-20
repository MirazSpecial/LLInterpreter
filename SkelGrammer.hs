-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelGrammer where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsGrammer

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsGrammer.Ident -> Result
transIdent x = case x of
  AbsGrammer.Ident string -> failure x

transArOp :: AbsGrammer.ArOp -> Result
transArOp x = case x of
  AbsGrammer.OpAdd -> failure x
  AbsGrammer.OpSub -> failure x
  AbsGrammer.OpMul -> failure x
  AbsGrammer.OpDiv -> failure x

transCompOp :: AbsGrammer.CompOp -> Result
transCompOp x = case x of
  AbsGrammer.OpEq -> failure x
  AbsGrammer.OpNeq -> failure x
  AbsGrammer.OpGr -> failure x
  AbsGrammer.OpGeq -> failure x
  AbsGrammer.OpLe -> failure x
  AbsGrammer.OpLeq -> failure x

transBoolOp :: AbsGrammer.BoolOp -> Result
transBoolOp x = case x of
  AbsGrammer.OpAnd -> failure x
  AbsGrammer.OpOr -> failure x

transExpr :: AbsGrammer.Expr -> Result
transExpr x = case x of
  AbsGrammer.ELitInt integer -> failure x
  AbsGrammer.ELitTrue -> failure x
  AbsGrammer.ELitFalse -> failure x
  AbsGrammer.EList exprs -> failure x
  AbsGrammer.EListHead expr -> failure x
  AbsGrammer.EListTail expr -> failure x
  AbsGrammer.EListEmpty expr -> failure x
  AbsGrammer.EArOp arop expr1 expr2 -> failure x
  AbsGrammer.ECompOp compop expr1 expr2 -> failure x
  AbsGrammer.EBoolOp boolop expr1 expr2 -> failure x
  AbsGrammer.EAnonFun ident idents expr -> failure x
  AbsGrammer.EApp ident exprs -> failure x
  AbsGrammer.EIf expr1 expr2 expr3 -> failure x
  AbsGrammer.EVar ident -> failure x
  AbsGrammer.ELet ident idents expr1 expr2 -> failure x
