module Main where 
import Control.Monad
import Control.Monad.Reader
import System.Environment
import System.IO
import Data.Functor

import AbsGrammer
import LexGrammer
import ParGrammer
import ErrM

import Interpreter
import Types

main :: IO ()
main = do
    progName:_ <- getArgs
    prog <- readFile progName
    case pExpr (myLexer prog) of
        Ok e -> print $ show (runReader (interpret e) (const Nothing))
        Bad s -> print $ show s