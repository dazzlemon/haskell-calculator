module Parser where

import Data.Maybe (listToMaybe)

-- expression ::= number
data Expression = SimpleExpression Number
--              | '(' expression ')'
                | ParenthesisExpression Expression
--              | function '(' expression{ ',' expression } ')'
								| FunctionCall Function [Expression]
--              | expression operator expression
								| OperatorCall Expression Operator Expression
--              | uint '!'
								| Factorial Number
								| EmptyExpression
                deriving (Show, Data, Eq)

data ParserError = UnexpectedToken { errorPosition::Int }
                 | ParserUnexpectedEOF

operatorPrecedence x
	| x `elem` [Multiply, Divide, Power] = 1
	| x `elem` [Plus, Minus] = 2

parse :: [TokenInfo] -> Either CalculatorError Expression -- TODO

getExpression :: [TokenInfo] -> Either CalculatorError (Expression, [TokenInfo])
getExpression [] = Right (EmptyExpression, [])
getExpression [TokenInfo pos single] = case single of
	TokenNumber number -> Right (SimpleExpression number, [])
	_ -> unexpectedToken pos
getExpression ((TokenInfo pos first):rest) = case first of
	TokenNumber number -> case head rest of
		TokenInfo _ (FactorialOperator fOp) -> Right (Factorial number, tail rest)
		TokenInfo _ (TokenOperator tOp) -> if tOp `elem` [Plus, Minus]
			then case getExpression $ tail rest of
				(s@(SimpleExpression _), rest')
					-> Right (OperatorCall number tOp s, rest')
				(oc@(OperatorCall e1 o e2), rest') -- change precedence
					-> if operatorPrecedence o > operatorPrecedence tOp
							then Right (OperatorCall number tOp oc, rest')
							else Right (OperatorCall (OperatorCall number tOp e1) o e2, rest')
				EmptyExpression -> Left $ CalculatorParserError $ ParserUnexpectedEOF
				_ -> case parse rest' of -- this is possible because
				                         -- we have only two levels of precedence
					err@(Left _) -> err
					Rigth expression -> Right (OperatorCall number tOp expression, [])
			else case getExpression $ tail rest of
				((OperatorCall e1 o e2), rest') -- change precedence to be left to right
					-> Right (OperatorCall (OperatorCall number tOp e1) o e2, rest')
				(expression, rest')
					-> Right (OperatorCall number tOp expression, rest')
				EmptyExpression -> Left $ CalculatorParserError $ ParserUnexpectedEOF
		TokenInfo pos _ -> unexpectedToken pos
	TokenInfo _ (TokenFunction Function) -> case head rest of
		TokenInfo pos ParenthesisLeft -> case getExpression $ tail rest of
			Right (expr, rest') -> getFunctionArgs rest' [expr]
			err@(Left _) -> err
		_ -> unexpectedToken pos
	TokenInfo _ ParenthesisLeft -> -- TODO: getExpression until hit ParenthesisRight
	_ -> unexpectedToken pos

getFunctionArgs :: [TokenInfo] -> [Expression]
                -> Either CalculatorError ([Expression], [TokenInfo])
getFunctionArgs (Comma:rest) args = case getExpression rest of
	Right (expr, rest') -> getFunctionArgs rest' (expr:args)
	err@(Left _) -> err
getFunctionArgs (ParenthesisRight:rest) args = Right (args, rest)
getFunctionArgs _ _ = unexpectedToken pos

unexpectedToken pos = Left $ CalculatorParserError $ UnexpectedToken pos