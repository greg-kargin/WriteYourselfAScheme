module Main where
import System.IO

main :: IO ()
main = do
  putStr "Enter your name: "
  hFlush stdout
  name <- getLine
  putStrLn $ "Nice to meet you, " ++ name
