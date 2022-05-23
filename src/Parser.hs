{-# LANGUAGE DeriveDataTypeable #-}

module Parser where

import Data.Maybe (listToMaybe)
import Data.Data (Typeable, Data)

import Common
import Lexer

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
								| NegativeExpression Expression
                deriving (Show, Data, Eq)

operatorPrecedence x
	| x `elem` [Multiply, Divide, Power] = 1
	| x `elem` [Plus, Minus] = 2

parser :: [TokenInfo] -> Either CalculatorError Expression
parser [] = Right EmptyExpression
-- only number can be the single token in expression
parser [TokenInfo pos single] = case single of
	TokenNumber number -> Right $ SimpleExpression number
	_ -> unexpectedToken pos

-- complex expressions:
--   OperatorCall
--   Factorial
parser ((TokenInfo p (TokenNumber lhs)):rest) =
	case getSimpleExpression ((TokenInfo p (TokenNumber lhs)):rest) of
		(rhs, restTokens) -> Right rhs

-- TODO: FunctionCall, ParenthesisExpression, NegativeExpression
-- parser ((TokenInfo _ (TokenFunction function)):rest) = case head rest of 

parser ((TokenInfo pos ParenthesisLeft):rest) =
	case getSimpleExpression rest of
		(expr, restTokens) -> case listToMaybe restTokens of
			Just (TokenInfo _ tok) -> case tok of
				ParenthesisRight -> case listToMaybe $ tail restTokens of
					Nothing -> Right $ ParenthesisExpression expr
					Just (TokenInfo _ tok') -> case tok' of
						TokenOperator op -> case getSimpleExpression $ drop 2 restTokens of
							(rhs, restTokens') -> case rhs of
								EmptyExpression -> Right $ ParenthesisExpression expr
								OperatorCall lhs' op' rhs' ->
									if operatorPrecedence op' > operatorPrecedence op
										then Right $ OperatorCall
											(OperatorCall (ParenthesisExpression expr) op lhs')
											op' rhs
										else Right $ OperatorCall (ParenthesisExpression expr) op rhs
			Nothing -> Left $ CalculatorParserError ParserUnexpectedEOF 0
			_ -> Left $ CalculatorParserError UnexpectedToken pos

-- expression can start with number, function name or left parenthesis, or minus
parser ((TokenInfo pos _):_) = Left $ CalculatorParserError UnexpectedToken pos

getSimpleExpression :: [TokenInfo] -> (Expression, [TokenInfo])
getSimpleExpression [TokenInfo pos (TokenNumber number)] =
	(SimpleExpression number, [])
getSimpleExpression ((TokenInfo _ (TokenNumber lhs)):rest) = case head rest of
	TokenInfo pos tok -> case tok of
		TokenOperator op -> case getSimpleExpression $ tail rest of
			(rhs, restTokens) -> case rhs of
				EmptyExpression -> (SimpleExpression lhs, restTokens)
				OperatorCall lhs' op' rhs' ->
					if operatorPrecedence op' > operatorPrecedence op
						then ( OperatorCall (OperatorCall (SimpleExpression lhs) op lhs') 
							                  op' rhs'
								 , restTokens)
						else (OperatorCall (SimpleExpression lhs) op rhs, restTokens)
				rhs -> (OperatorCall (SimpleExpression lhs) op rhs, restTokens)
		FactorialOperator -> case listToMaybe $ tail rest of
			Nothing -> (Factorial lhs, [])
			Just (TokenInfo pos' tok') -> case tok' of
				-- drop FactorialOperator and TokenOperator to parse rhs
				TokenOperator op -> case getSimpleExpression $ drop 2 rest of
					(rhs, restTokens) -> case rhs of
						EmptyExpression -> (Factorial lhs, restTokens)
						OperatorCall lhs' op' rhs' ->
							if operatorPrecedence op' > operatorPrecedence op
								then ( OperatorCall (OperatorCall (Factorial lhs) op lhs')
								                    op' rhs
										 , restTokens)
								else (OperatorCall (Factorial lhs) op rhs, restTokens)
						rhs -> (OperatorCall (SimpleExpression lhs) op rhs, restTokens)
getSimpleExpression rest = (EmptyExpression, rest)

unexpectedToken pos = Left $ CalculatorParserError UnexpectedToken pos
