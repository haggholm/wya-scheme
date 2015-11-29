module Main where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escapeSeq :: Parser Char
escapeSeq = do
                char '\\'
                e <- oneOf "nrt\\\""
                return $ case e of
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'
                    '"'  -> '\"'
                    '\\' -> '\\'

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Character Char
             | String String
             | Bool Bool

parseCharacter :: Parser LispVal
parseCharacter = do
                char '\''
                x <- escapeSeq <|> noneOf "\""
                char '\''
                return $ Character x

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (escapeSeq <|> noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first : rest
                return $ case atom of
                    "#t" -> Bool True
                    "#f" -> Bool False
                    _    -> Atom atom

radixNumber :: Parser Integer
radixNumber = do
                char '#'
                base <- oneOf "bdox"
                num <- many1 digit
                return $ (fst . (!! 0)) $ case base of
                    'b' -> (readInt 2 (\x -> x `elem` "01") (\x -> read [x])) num
                    'd' -> readDec num
                    'o' -> readOct num
                    'x' -> readHex num

bareNumber :: Parser Integer
bareNumber = (many1 digit) >>= (return . read)

parseNumber :: Parser LispVal
parseNumber = do
                num <- radixNumber <|> bareNumber
                return $ Number num
-- parseNumber = do
--                 many1 digit
--                 return $ (Number . read) numStr


parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err    -> "No match: " ++ show err
    Right val   -> "Found value"



main :: IO ()
main = do
    (expr : _) <- getArgs
    putStrLn expr
    putStrLn (readExpr expr)

