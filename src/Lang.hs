

module R1.Lang where

import qualified Data.Text as T
import Data.Char

data R1Expr = R1Int Int
            | R1Add R1Expr R1Expr
            | R1Neg R1Expr
            | R1Var T.Text
            | R1Read
            | R1Let R1Expr R1Expr R1Expr deriving (Eq)

newtype R1 = Program (String, R1Expr) deriving (Show)

instance Show R1Expr where
    show (R1Int x) = show x
    show R1Read    = "(read)"
    show (R1Add x y) = "(+ " ++ show x ++ " " ++ show y ++ ")"
    show (R1Neg x) = "(- " ++ show x ++ ")"
    show (R1Var x) = T.unpack x
    show (R1Let var val exp) = "(let ([" ++ show var ++ " " ++ show val ++ "]) " ++ show exp ++ ")"




isR1Var :: R1Expr -> Bool
isR1Var (R1Var _) = True -- weak check 
isR1Var        _  = False

isR1Expr :: R1Expr -> Bool
isR1Expr (R1Int _) = True
isR1Expr (R1Var _) = True
isR1Expr (R1Add x y) = isR1Expr x && isR1Expr y
isR1Expr (R1Neg x) = isR1Expr x
isR1Expr (R1Let var value exp) = isR1Var var && isR1Expr value && isR1Expr exp


type R1Env = [(T.Text, R1Expr)]

extEnv :: T.Text -> R1Expr -> R1Env -> R1Env
extEnv var val env = ((var, val) : env)

emptyR1Env :: R1Env
emptyR1Env = []


