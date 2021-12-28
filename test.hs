import R1.Lang
import Text.ParserCombinators.Parsec
import R1.Parser
import R1.Interpreter



fromRight (Right x) = x

example = "(let ([x (+ 24 18)]) (let ([y (read)]) (- (+ y x))))"

example_exp = fromRight $ parse pExpr "R1" example

main = evalR1 [] example_exp >>= print
