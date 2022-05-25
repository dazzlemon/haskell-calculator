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
								| NegativeExpression Expression
                deriving (Show, Data, Eq)

-- lower first
operatorPrecedence x
	| x `elem` [Multiply, Divide, Power] = 1
	| x `elem` [Plus, Minus] = 2

-- only works for numbers and operators (not unary minus)
parser :: [Token] -> Either CalculatorError Expression
parser tokens = case listToMaybe tokens of
	Just (TokenNumber number) -> case parser' (SimpleExpression number) (tail tokens) of
		Left err -> Left err
		Right (ast, []) -> Right ast
	_ -> Left $ CalculatorParserError UnexpectedToken 0

parser' :: Expression -> [Token] -> Either CalculatorError (Expression, [Token])
parser' lhs rest = case listToMaybe rest of
	Nothing -> Right (lhs, [])
	Just (TokenOperator op) -> case listToMaybe $ tail rest of
		-- operator doesn't have rhs
		Nothing -> Left $ CalculatorParserError UnexpectedToken 0
		Just (TokenNumber rhs) -> case listToMaybe $ drop 2 rest of
			Nothing -> Right (OperatorCall lhs op (SimpleExpression rhs), [])
			Just (TokenOperator op') ->
				if operatorPrecedence op <= operatorPrecedence op'
					then parser' (OperatorCall lhs op (SimpleExpression rhs)) (drop 2 rest)
					else case subExp of
						Left err -> Left err
						Right (rhs', rest') -> parser' (OperatorCall lhs op rhs') rest'
					where subExp = parseUntilPrecedence (operatorPrecedence op) (SimpleExpression rhs) (drop 2 rest)
			_ -> Left $ CalculatorParserError UnexpectedToken 0
		_ -> Left $ CalculatorParserError UnexpectedToken 0
	_ -> Left $ CalculatorParserError UnexpectedToken 0

parseUntilPrecedence :: Int -> Expression -> [Token]
                     -> Either CalculatorError (Expression, [Token])
parseUntilPrecedence n lhs rest = case listToMaybe rest of
	Nothing -> Right (lhs, [])
	Just (TokenOperator op) -> if operatorPrecedence op >= n
					then Right (lhs, rest)
					else case listToMaybe $ tail rest of
		-- operator doesn't have rhs
						Nothing -> Left $ CalculatorParserError UnexpectedToken 0
						Just (TokenNumber rhs) -> case listToMaybe $ drop 2 rest of
							Nothing -> Right (OperatorCall lhs op (SimpleExpression rhs), [])
							Just (TokenOperator op') ->
								if operatorPrecedence op <= operatorPrecedence op'
									then parseUntilPrecedence n (OperatorCall lhs op (SimpleExpression rhs)) (drop 2 rest)
									else Right (lhs, rest)
						_ -> Left $ CalculatorParserError UnexpectedToken 0
	_ -> Left $ CalculatorParserError UnexpectedToken 0

-- 1) operator precedence

-- 1 + 2 + 3 * 4 * 5 - 6 + 7
-- (1 + 2) + 3 * 4 * 5 - 6 + 7
-- (1 + 2) + (3 * 4) * 5 - 6 + 7
-- (1 + 2) + ((3 * 4) * 5) - 6 + 7
-- ((1 + 2) + ((3 * 4) * 5)) - 6 + 7
-- (((1 + 2) + ((3 * 4) * 5)) - 6) + 7
-- ((((1 + 2) + ((3 * 4) * 5)) - 6) + 7)
-- 64

-- Plus
-- 				Minus
-- 								Plus
-- 												Plus
-- 																1
-- 																2
-- 												Multiply
-- 																Multiply
-- 																				3
-- 																				4
-- 																5
-- 								6
-- 				7