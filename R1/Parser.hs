module R1.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.Text as T
import Control.Monad

import R1.Lang

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

pVar :: Parser R1Expr
pVar = do { x <- symbol <|> letter;
            xs <- many (symbol <|> letter <|> digit);
            return $ R1Var (T.pack (x:xs))
          }

pInt :: Parser R1Expr
pInt = liftM (R1Int . read) $ many1 digit

pRead :: Parser R1Expr
pRead = string "(read)" *> return R1Read


pAdd , pNeg:: Parser R1Expr
pAdd = string "(+" *> spaces *> do { x <- pExpr; spaces; y <- pExpr; char ')'; return (R1Add x y)} 
pNeg = string "(-" *> spaces *> do { x <- pExpr; char ')'; return (R1Neg x)}

pLet :: Parser R1Expr
pLet = string "(let" *> spaces *> string "([" *> do { var <- pVar; char ' '; value <- pExpr; char ']'; char ')'; spaces; exp <- pExpr; return (R1Let var value exp)}

-- 合规的R1表达式应该可以被racket解释器解释 
pExpr :: Parser R1Expr
pExpr = choice $ try <$> [pInt, pVar, pLet, pAdd, pNeg, pRead]
