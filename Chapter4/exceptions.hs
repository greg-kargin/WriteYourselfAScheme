module Main where
import Numeric
import Data.Char
import Data.Typeable
import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn $ show $ eval $ (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr input = case parse parseExpr "lisp" input of
    Left err -> String ("No match: " ++ show err)
    Right val -> val

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

instance Show LispVal where show = showVal

primitives :: [(String, [LispVal] ->  LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", checkType isSym),
              ("string?", checkType isStr),
              ("number?", checkType isNum),
              ("float?",  checkType isFlt),
              ("symbol->string", symbol2string),
              ("string->symbol", string2symbol)
             ]

symbol2string :: [LispVal] -> LispVal
symbol2string ((Atom sym):[]) = String sym
symbol2string _ = String "" -- later should be changed to error

string2symbol :: [LispVal] -> LispVal
string2symbol ((String sym):[]) = Atom sym
string2symbol _ = Atom "" -- same

checkType :: (LispVal -> Bool) -> [LispVal] -> LispVal
checkType check syms = Bool (foldl1 (&&) $ map check syms)

isSym :: LispVal -> Bool
isSym s = case s of
  Atom _ -> True
  _      -> False

isStr :: LispVal -> Bool
isStr s = case s of
  String _ -> True
  _        -> False

isNum :: LispVal -> Bool
isNum n = case n of
  Number _ -> True
  _        -> False

isFlt :: LispVal -> Bool
isFlt n = case n of
  Float _ -> True
  _       -> False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

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
                 case first of
                   '#' -> do second <- oneOf "bodh"
                             rest <- many digit
                             return $ matchBase (first:second:rest)
                   _   -> do rest <- many digit
                             return $ matchBase (first:rest)

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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

showVal :: LispVal -> String
showVal (String c) = "\"" ++ c ++ "\""
showVal (Atom a) = a
showVal (Number n) = show n
showVal (Character c) = show c
showVal (Float f) = show f
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "."
  ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval (String c) = (String c)
eval (Atom a) = (Atom a)
eval (Number n) = (Number n)
eval (Float f) = (Float f)
eval (Character c) = (Character c)
eval (Bool b) = (Bool b)
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
