module Main where
import Numeric
import Data.Char
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ many1 (noneOf "\"\\") <|> escapeChars
                 char '"'
                 return $ String (concat x)

escapeChars :: Parser String
escapeChars = do char '\\'
                 x <- oneOf "\\\"ntr"
                 case x of
                   '\\' -> do return [x]
                   '"'  -> do return [x]
                   't'  -> do return "\t"
                   'r'  -> do return "\r"
                   'n'  -> do return "\n"

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $  Atom atom

parseBool :: Parser LispVal
parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          'f' -> Bool False

readBin :: String -> Integer
readBin [] = 0
readBin ('0':num) = readBin num
readBin ('1':num) = 2 ^ (length num) + readBin num

matchBase ('#':'b':rest) = Number $ readBin rest
matchBase ('#':'o':rest) = Number $ fst $ readOct rest !! 0
matchBase ('#':'x':rest) = Number $ fst $ readHex rest !! 0
matchBase ('#':'d':rest) = Number $ read rest
matchBase decimalNumber  = Number $ read decimalNumber

parseNumber :: Parser LispVal
parseNumber = do first <- digit <|> char '#'
                 second <- digit <|> oneOf "bodh"
                 rest <- many digit
                 return $ matchBase (first:second:rest)

parseFloat :: Parser LispVal
parseFloat = do h <- many digit
                c <- char '.'
                t <- many digit
                return $ Float $ fst.head $ readFloat (h ++ "." ++ t)

parseCharacter :: Parser LispVal
parseCharacter = do string "#\\"
                    c <- parseCharName <|> anyChar
                    return $ Character c

parseCharName = do name <- string "space" <|> string "newline"
                   return $ case map toLower name of
                              "space" -> ' '
                              "newline" -> '\n'

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
