module Types where 

import Control.Monad.Reader

import AbsGrammer


data Value = 
    VInt Integer |
    VBool Bool |
    VList [Value] |
    VFun (Value -> Value)

instance Eq Value where 
    VInt n1 == VInt n2 = n1 == n2
    VBool b1 == VBool b2 = b1 == b2
    VList l1 == VList l2 = l1 == l2 
    VFun f1 == VFun f2 = False

instance Show Value where
    show (VInt i)  = show i
    show (VBool b) = show b
    show (VList l) = show l
    show (VFun _)  = "<function>"


type Binding = Ident -> Maybe Value
type ReaderVars = Reader Binding