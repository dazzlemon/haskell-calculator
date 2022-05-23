{-# LANGUAGE DeriveDataTypeable #-}

module Common where

import Data.Data (Typeable, Data)

data CalculatorError = CalculatorLexerError     { lexerError::LexerError
                                                , errorPosition::Int }
                     | CalculatorParserError    { parserError::ParserError
										                            , errorPosition::Int }
										 | CalculatorEvaluatorError { errorPosition::Int }
                     deriving (Show, Data, Eq)

data ParserError = UnexpectedToken
								 | ParserUnexpectedEOF
									deriving (Show, Data, Eq)


data LexerError = UnknownSymbol
                | UnexpectedSymbol { expected::String }
                | UnexpectedEOF { expected::String }
								| UnknownWord
                deriving (Show, Data, Eq)