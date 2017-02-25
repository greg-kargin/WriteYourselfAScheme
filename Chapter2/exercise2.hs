import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseString' :: Parser LispVal
parseString' = do char '"'
                  x <- many $ many1 (noneOf "\"\\") <|> escapeChars
                  char '"'
                  return $ String (concat x)

escapeChars :: Parser String
escapeChars = do char '\\'
                 x <- oneOf "\\\""
                 return [x]
