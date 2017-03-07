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
          evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
          putStrLn $ extractValue $ trapError $ evaled

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

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

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

symbol2string :: [LispVal] -> ThrowsError LispVal
symbol2string ((Atom sym):[]) = return $ String sym
symbol2string [notSymbol] = throwError $ TypeMismatch "symbol" notSymbol

string2symbol :: [LispVal] -> ThrowsError LispVal
string2symbol ((String sym):[]) = return $ Atom sym
string2symbol [notString] = throwError $ TypeMismatch "string" notString

checkType :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
checkType check syms = return $ Bool (foldl1 (&&) $ map check syms)

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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@([ _ ]) = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

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

eval :: LispVal -> ThrowsError LispVal
eval (String c) = return (String c)
eval (Atom a) = return (Atom a)
eval (Number n) = return (Number n)
eval (Float f) = return (Float f)
eval (Character c) = return (Character c)
eval (Bool b) = return (Bool b)
eval (List [Atom "quote", val ]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form: "  badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognized primitive function args" func )
  ($ args)
  (lookup func primitives)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (NumArgs expected found) =
  "Expected: " ++ show expected ++ ", found: " ++ show found
showError (TypeMismatch expected found) =
  "Invalid type, expected: " ++ expected ++ ", found: " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
instance Error LispError where
  noMsg = Default "An error has occured"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
