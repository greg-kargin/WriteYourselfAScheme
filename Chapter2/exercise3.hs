import Text.ParserCombinators.Parsec

escapeChars' :: Parser String
escapeChars' = do char '\\'
                  x <- oneOf "\\\"ntr"
                  case x of
                    '\\' -> do return [x]
                    '"'  -> do return [x]
                    't'  -> do return "\t"
                    'r'  -> do return "\r"
                    'n'  -> do return "\n"
