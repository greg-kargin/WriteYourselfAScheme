import Numeric
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

readBin :: String -> Integer
readBin [] = 0
readBin ('0':num) = readBin num
readBin ('1':num) = 2 ^ (length num) + readBin num

matchBase ('#':'b':rest) = Number $ readBin rest
matchBase ('#':'o':rest) = Number $ fst $ readOct rest !! 0
matchBase ('#':'h':rest) = Number $ fst $ readHex rest !! 0
matchBase decimalNumber  = Number $ read decimalNumber

parseNumber' :: Parser LispVal
parseNumber' = do first <- digit <|> char '#'
                  second <- digit <|> oneOf "bodh"
                  rest <- many digit
                  return $ matchBase (first:second:rest)
