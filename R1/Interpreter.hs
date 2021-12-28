module R1.Interpreter where

import R1.Lang
import Control.Applicative
import qualified Data.Text as T

evalR1 :: R1Env -> R1Expr -> IO Int
evalR1 _ (R1Int n) = return n
evalR1 e (R1Neg exp) = return (`subtract` 0) <*> evalR1 e exp
evalR1 e (R1Var v) = case lookup v e of { Nothing -> error $ "variable " ++ T.unpack v ++ " : not found in context"; Just (R1Int n) -> return n}
evalR1 e (R1Add x y) = liftA2 (+) (evalR1 e x) (evalR1 e y)
evalR1 _ R1Read = getLine >>= (\number -> return (read number :: Int))
evalR1 e (R1Let (R1Var var) value exp) = evalR1 e value >>= (\val -> evalR1 (extEnv var (R1Int val) e) exp)


