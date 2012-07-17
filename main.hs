module Main where
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
  line <- getLine
  putStrLn $ expr line

symbol :: Parser Char
symbol = oneOf "+-*/%<>=&|^_~#:?$!@"

expr :: String -> String
expr input = case parse (spaces >> symbol) "scheme" input of
  Left err -> show err
  Right val -> show val

spaces :: Parser ()
spaces = skipMany1 space

data Val = Atom String
         | List [LispVal]
         | DottedList [LispVal] LispVal
         | Number Integer
         | String String
         | Bool Bool

string :: Parser Val
string = do char '"'
            x <- many (noneOf "\"")
            char '"'
            return $ String x
