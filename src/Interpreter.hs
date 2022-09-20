module Interpreter where

import Control.Monad.Reader
import Data.Typeable
import Debug.Trace

import AbsGrammer
import ErrM

import Types

parseList :: [Expr] -> ReaderVars Value
parseList [] = return $ VList []
parseList (e1:es) = do 
    esVal <- local id (parseList es)
    e1val <- local id (interpret e1)
    case esVal of 
        VList l -> return $ VList (e1val:l)
        _ -> undefined

applyArg :: Value -> Value -> Value
applyArg f arg = case f of 
    VFun fun -> fun arg 
    _ -> error "Too many arguments applied"

namedFunParse :: Expr -> ReaderVars Value
namedFunParse (ELet f [x] e1 _) = do 
    env <- ask
    let funVal xVal = (runReader (interpret e1) (\y -> 
            if y == x then Just xVal
            else if y == f then Just $ VFun funVal 
            else env y))
    return $ VFun funVal
namedFunParse (ELet f (x:xs) e1 e2) = do 
    env <- ask 
    let funVal xVal = (runReader (namedFunParse (ELet f xs e1 e2)) (\y ->
            if y == x then Just xVal 
            else env y))
    return $ VFun funVal
namedFunParse _ = undefined


interpret :: Expr -> ReaderVars Value
interpret (ELitInt n) = return $ VInt n
interpret (ELitTrue) = return $ VBool True
interpret (ELitFalse) = return $ VBool False
interpret (EList l) = do
    env <- ask
    case runReader (parseList l) env of
        VList lParsed -> return $ VList lParsed
        _ -> undefined -- this does not happen
interpret (EListHead e) = do 
    eVal <- local id (interpret e)
    case eVal of 
        VList [] -> error "Empty list"
        VList (x:_) -> return x
        _ -> error "Types error"
interpret (EListTail e) = do 
    eVal <- local id (interpret e)
    case eVal of 
        VList [] -> error "Empty list"
        VList (_:xs) -> return $ VList xs 
        _ -> error "Types error"
interpret (EListEmpty e) = do 
    eVal <- local id (interpret e)
    case eVal of
        VList l -> return $ VBool (null l)
        _ -> error "Types error"
interpret (EArOp o e1 e2) = do
    e1val <- local id (interpret e1)
    e2val <- local id (interpret e2)
    case (e1val, e2val) of 
        (VInt n1, VInt n2) ->
            case o of 
                OpAdd -> return $ VInt (n1 + n2)
                OpSub -> return $ VInt (n1 - n2)
                OpMul -> return $ VInt (n1 * n2)
                OpDiv -> if n2 == 0 
                    then error "Division by zero!"
                    else return $ VInt (div n1 n2)
        (_, _) -> error "Types error"
interpret (ECompOp o e1 e2) = do 
    e1val <- local id (interpret e1)
    e2val <- local id (interpret e2)
    case (e1val, e2val) of 
        (VInt n1, VInt n2) ->
            case o of 
                OpEq -> return $ VBool (n1 == n2)
                OpNeq -> return $ VBool (n1 /= n2)
                OpGr -> return $ VBool (n1 > n2)
                OpGeq -> return $ VBool (n1 >= n2)
                OpLe -> return $ VBool (n1 < n2)
                OpLeq -> return $ VBool (n1 <= n2)
        (VBool b1, VBool b2) ->
            case o of 
                OpEq -> return $ VBool (b1 == b2)
                OpNeq -> return $ VBool (b1 /= b2)
                _ -> error "Types error"
        (VList l1, VList l2) ->
            case o of 
                OpEq -> return $ VBool (l1 == l2)
                OpNeq -> return $ VBool (l1 /= l2)
                _ -> error "Types error"
        (_, _) -> error "Types error"
interpret (EBoolOp o e1 e2) = do
    e1val <- local id (interpret e1)
    e2val <- local id (interpret e2)
    case (e1val, e2val) of 
        (VBool b1, VBool b2) ->
            case o of 
                OpAnd -> return $ VBool (b1 && b2)
                OpOr -> return $ VBool (b1 || b2)
        (_, _) -> error "Types error"
interpret (EIf e1 e2 e3) = do 
    e1val <- local id (interpret e1)
    case e1val of 
        VBool True -> interpret e2 
        VBool False -> interpret e3 
        _ -> error "Types error"
interpret (EVar x) = do 
    env <- ask
    case env x of 
        Just xVal -> return xVal
        Nothing -> error ("Undefined variable: " ++ show x)
interpret (ELet x [] e1 e2) = do 
    e1val <- local id (interpret e1)
    local (\env y -> if y == x then Just e1val else env y) (interpret e2)

interpret (ELet f xs e1 e2) = do
    env <- ask 
    let VFun funVal  = (runReader (namedFunParse (ELet f xs e1 e2)) env)
    local (\env y -> if y == f then Just $ VFun funVal else env y) (interpret e2)

interpret (EAnonFun x [] e) = do
    env <- ask
    let funVal xVal = (runReader (interpret e) (\y -> if y == x then Just xVal else env y))
    return $ VFun funVal
interpret (EAnonFun x (x1:xs) e) = do 
    env <- ask
    let funVal xVal = (runReader (interpret (EAnonFun x1 xs e)) (\y -> 
            if y == x then Just xVal 
            else env y))
    return $ VFun funVal
interpret (EApp f es) = do
    argsParsed <- local id (interpret $ EList es)
    env <- ask
    case (env f, argsParsed) of 
        (Just (VFun fun), VList argsList) -> return $ foldl applyArg (VFun fun) argsList
        (Nothing, _) -> error ("Undefined function: " ++ show f)
        (_, _) -> undefined