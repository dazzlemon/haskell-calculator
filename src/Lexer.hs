{-# LANGUAGE DeriveDataTypeable #-}
module Lexer where

import Data.Data (Typeable, Data)
import Data.Char (isAlpha, isNumber, isAlphaNum, isSpace)
import Control.Applicative (liftA2)
import Data.Maybe (listToMaybe)

-- digit ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
-- uint ::= digit{digit}
-- number ::= ['-'] uint | ([uint]'.'uint)

-- operator ::= '+' | '-' | '*' | '/' | '^'

-- function ::= 'sin' | 'cos' | 'tan' | 'cot' | 'sqrt' | 'root'

data Token = TokenNumber Number
           | TokenOperator Operator
					 | TokenFunction Function
           | ParenthesisLeft
           | ParenthesisRight
					 | Comma
					 | TokenEOF
           | FactorialOperator
           deriving (Show, Data, Eq)

tokenLength :: Token -> Int
tokenLength tokenNumber = case tokenNumber of
  TokenNumber (Number str) -> length str
  TokenFunction function -> case function of
    SquareRoot -> 4
    Root -> 4
    _ -> 3
  _ -> 1

newtype Number = Number String deriving (Show, Data, Eq)

data Operator = Plus
              | Minus
							| Multiply
							| Divide
							| Power
              deriving (Show, Data, Eq)

data Function = Sine
              | Cosine
							| Tangent
							| Cotangent
							| SquareRoot
							| Root
              deriving (Show, Data, Eq)

data CalculatorError = CalculatorLexerError LexerError
                     | CalculatorParserError ParserError
										 | CalculatorEvaluatorError
                     deriving (Show, Data, Eq)
						
data LexerError = UnknownSymbol    { errorPosition::Int }
                | UnexpectedSymbol { errorPosition::Int
                                   , expected::String }
                | UnexpectedEOF    { expected::String }
								| UnknownWord      { errorPosition::Int }
                deriving (Show, Data, Eq)

data TokenInfo = TokenInfo { position::Int, token::Token } deriving (Eq, Show)

lexer :: String -> Either CalculatorError [TokenInfo]
lexer expression = lexer' expression 0 []

lexer' :: String -> Int -> [TokenInfo] 
       -> Either CalculatorError [TokenInfo]
lexer' [] _ tokens = Right tokens
lexer' expression position tokens = case getToken expression position of
  -- repeat until TokenEOF or err
  Right (tokenInfo@(TokenInfo position token), rest) -> case token of
    TokenEOF -> Right tokens
    _ -> lexer' rest (position + tokenLength token) (tokens ++ [tokenInfo])
  Left err -> Left err

getToken :: String -> Int -> Either CalculatorError (TokenInfo, String)
getToken [] position = Right (TokenInfo position TokenEOF, [])
getToken (firstChar:code) position
  | firstChar == '-' = case listToMaybe code of
    -- TODO: cleanup this branch
    -- +1 because skipping '-'
    Just x | isNumber x || x == '.' -> case getNumber code (position + 1) of
      -- don't care for the position, because it starts where we said it to
      Right (TokenInfo _ (TokenNumber (Number positive)), rest) ->
        returnToken (TokenNumber (Number ('-':positive))) rest
      err@(Left _) -> err -- after '.'
      _ -> returnError (UnexpectedSymbol position "numeric") -- after '-'
    -- +1 -> is '-'; +2 is after '-'
    Just x -> returnError (UnexpectedSymbol (position + 2) "numeric")
    _ -> returnError $ UnexpectedEOF "numeric"
  | isSpace firstChar = getToken code (position + 1) -- +1 -> skip char
  | isNumber firstChar || firstChar == '.' = getNumber (firstChar:code) position
  | isAlpha firstChar = case wordToToken word of
          Just token -> returnToken token restAfterWord
          _ -> returnError (UnknownWord position)
  | Just token <- charToToken firstChar = returnToken token code
  | otherwise = returnError (UnknownSymbol position)
  where returnToken token rest = Right (TokenInfo position token, rest)
        (wordRest, restAfterWord) = span isAlphaNumOrUnderscore code
        returnError err = Left (CalculatorLexerError err)
        word = firstChar:wordRest

charToToken :: Char -> Maybe Token
charToToken = flip lookup [ ('(', ParenthesisLeft)
                          , (')', ParenthesisRight)
													, ('+', TokenOperator Plus)
                          , ('-', TokenOperator Minus)
							            , ('*', TokenOperator Multiply)
							            , ('/', TokenOperator Divide)
							            , ('^', TokenOperator Power)
													, (',', Comma)
                          , ('!', FactorialOperator)
                          ]

wordToToken :: String -> Maybe Token
wordToToken = flip lookup [ ("sin", TokenFunction Sine)
                          , ("cos", TokenFunction Cosine)
							            , ("tan", TokenFunction Tangent)
							            , ("cot", TokenFunction Cotangent)
							            , ("sqrt", TokenFunction SquareRoot)
							            , ("root", TokenFunction Root)
                          ]

-- only accepts positive numbers in formats like: 1, 1.2, .2
getNumber :: String -> Int -> Either CalculatorError (TokenInfo, String)
getNumber code position = case afterWhole of
  "." -> returnError $ UnexpectedEOF "numeric"
  '.':_ -> if null fraction
    then returnError (UnexpectedSymbol (position + length whole + 1) "numeric")
    else Right 
			( TokenInfo position (TokenNumber (Number (whole ++ "." ++ fraction)))
			, afterFraction)
  _ -> Right (TokenInfo position (TokenNumber (Number whole)), afterWhole)
  where (whole, afterWhole) = span isNumber code
        (fraction, afterFraction) = span isNumber
                                  $ drop 1 afterWhole -- drop '.'
        returnError err = Left (CalculatorLexerError err)		

isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore = liftA2 (||) isAlphaNum (== '_')