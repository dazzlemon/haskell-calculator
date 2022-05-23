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
parser ((TokenInfo _ (TokenNumber lhs)):rest) = case head rest of
	TokenInfo pos tok -> case tok of
		TokenOperator op -> case parser $ tail rest of
			err@(Left _) -> err
			Right rhs -> case rhs of
				EmptyExpression -> Right $ SimpleExpression lhs
				OperatorCall lhs' op' rhs' ->
					if operatorPrecedence op' > operatorPrecedence op
						then Right $ OperatorCall (SimpleExpression lhs) op rhs
						else Right $ OperatorCall
							(OperatorCall (SimpleExpression lhs) op lhs') op' rhs'
				rhs -> Right $ OperatorCall (SimpleExpression lhs) op rhs
		FactorialOperator -> case listToMaybe $ tail rest of
			Nothing -> Right $ Factorial lhs
			Just (TokenInfo pos' tok') -> case tok' of
				-- drop FactorialOperator and TokenOperator to parse rhs
				TokenOperator op -> case parser $ drop 2 rest of
					err@(Left _) -> err
					Right rhs -> case rhs of
						EmptyExpression -> Right $ Factorial lhs
						OperatorCall lhs' op' rhs' ->
							if operatorPrecedence op' > operatorPrecedence op
								then Right $ OperatorCall (Factorial lhs) op rhs
								else Right $
									OperatorCall (OperatorCall (Factorial lhs) op lhs') op' rhs
						rhs -> Right $ OperatorCall (SimpleExpression lhs) op rhs

-- TODO: FunctionCall, ParenthesisExpression
-- parser ((TokenInfo _ (TokenFunction function)):rest) = case head rest of 

-- expression can start with number, function name or left parenthesis
parser _ = Left $ CalculatorParserError ParserUnexpectedEOF 0

unexpectedToken pos = Left $ CalculatorParserError UnexpectedToken pos