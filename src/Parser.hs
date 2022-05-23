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

getExpression :: [TokenInfo] -> Either CalculatorError (Expression, [TokenInfo])
getExpression [] = Right (EmptyExpression, [])
getExpression [TokenInfo pos single] = case single of
	TokenNumber number -> Right (SimpleExpression number, [])
	_ -> unexpectedToken pos
getExpression ((TokenInfo pos first):rest) = case first of
	TokenNumber number -> case head rest of
		TokenInfo _ (FactorialOperator fOp) -> Right (Factorial number, tail rest)
		TokenInfo _ (TokenOperator tOp) -> if top `elem` [Plus, Minus]
			then -- TODO: lookahead
			else case getExpression $ tail rest of
				(s@(SimpleExpression _), rest')
					-> Right (OperatorCall number tOp s, rest')
				(p@(ParenthesisExpression _), rest') 
					-> Right (OperatorCall number tOp p, rest')
				(fc@(FunctionCall _ _), rest')
					-> Right (OperatorCall number tOp fc, rest')
				((OperatorCall e1 o e2), rest') -- change precedence to be left to right
					-> Right (OperatorCall (OperatorCall number tOp e1) o e2, rest')
				(f@(Factorial _), rest')
					-> Right (OperatorCall number tOp f, rest')
				EmptyExpression -> Left $ CalculatorParserError $ ParserUnexpectedEOF
		TokenInfo pos _ -> unexpectedToken pos
	TokenFunction Function -> -- TODO:
  ParenthesisLeft -> -- TODO:
	_ -> unexpectedToken pos

unexpectedToken pos = Left $ CalculatorParserError $ UnexpectedToken pos