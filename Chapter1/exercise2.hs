module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "SUM = " ++ (show $ sum $ map read args)
  putStrLn $ "MUL = " ++ (show $ foldl1 (*) $ map read args)
