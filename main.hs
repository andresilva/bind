module Main where
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  line <- getLine
  putStrLn $ expr line

symbol :: Parser Char
symbol = oneOf "+-*/%<>=&|^_~#:?$!@"

expr :: String -> String
expr input = case parse symbol "scheme" input of
  Left err -> show err
  Right val -> show val