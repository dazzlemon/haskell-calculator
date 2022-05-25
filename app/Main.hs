module Main where

import Lexer
import Common
import Parser

import Data.Data (Data(toConstr))
import Data.List (transpose, elemIndices)
import Control.Monad ( when )
import System.Exit (exitFailure)
import ListPadding (rpad, lpad)
import System.Environment

main = do -- pretty print version
	args <- getArgs
	if length args /= 1 then print "Only one argument allowed"
	                    else calculator $ head args
	where calculator code = case lexer code of
					Left err -> case err of
						CalculatorLexerError lexErr _ -> do
							putStrLn $ "Error: " ++ constrString
							-- UnexpectedEOF only happens at the end of file))
							when (constrString /= "UnexpectedEOF") $
								putStr $ showTable [ [lineIndexStr, " | ", line]
																	 , ["",           "",    arrow]
																	 ]
							when (constrString == "UnexpectedSymbol") $
								putStrLn $ expected lexErr
							when (constrString == "UnexpectedEOF") $
								putStrLn $ expected lexErr
							exitFailure
							where constrString = show $ toConstr lexErr
							      lineIndexStr = show $ lineNumber + 1 -- 0 based + 1
							      lineStart = last linesStarts
							      line = takeWhile (/= '\n')
							      			$ drop lineStart code -- drop lines before 
							      arrow = lpad (charInLinePos + 1) ' ' "^"
							      offset = errorPosition err
							      linesStarts = 0:map (+1) ( drop 1 -- no line after last '\n'
							      																	-- 0 - start of first line
							      													$ elemIndices '\n'
							      													-- drop other lines
							      													$ take offset code)
							      charInLinePos = offset - lineStart
							      lineNumber = length linesStarts
						_ -> print "TODO: other error"
					Right tokenList -> do
						putStr $ showTable
									 $ tokenInfoListToTable tokenList
						case ast of
							Right ast' -> printAst ast' 0
							_ -> return ()
						print ast
						where ast = parser $ map token tokenList
						      printAst ast n = case ast of
											OperatorCall lhs op rhs -> do
												putStr $ replicate n '\t'
												print op
												printAst lhs (n + 1)
												printAst rhs (n + 1)
											SimpleExpression (Number str) -> do
												putStr $ replicate n '\t'
												putStrLn str
											_ -> return ()

tokenInfoListToTable :: [TokenInfo] -> [[String]]
tokenInfoListToTable = map tokenInfoToRow
  where tokenInfoToRow (TokenInfo position token) = [ positionString
                                                    , constrString
                                                    , extendedInfo
                                                    ]
          where constrString = show $ toConstr token
                extendedInfo = case token of
                  TokenNumber (Number string) -> "\'" ++ string ++ "\'"
                  TokenOperator op -> show $ toConstr op
                  TokenFunction f  -> show $ toConstr f
                  _ -> ""
                positionString = show position

showTable :: [[String]] -> String
showTable table = unlines $ map showRow table
  where maxLengths = map (maximum . map length) $ transpose table
        showRow = unwords . zipWith (`rpad` ' ') maxLengths