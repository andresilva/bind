module Main where
import Control.Monad
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec (Parser, (<|>), char,
                                      digit, letter, many, many1,
                                      noneOf, oneOf, parse, skipMany1,
                                      space)

main :: IO ()
main = do
  line <- getLine
  putStrLn $ eval line

eval :: String -> String
eval input = case parse expr "scheme" input of
  Left err -> show err
  Right val -> show val

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "+-*/%<>=&|^_~#:?$!@"

data Val = Atom String
         | List [Val]
         | DottedList [Val] Val
         | Number Integer
         | String String
         | Bool Bool
         deriving (Show)

string :: Parser Val
string = do char '"'
            x <- many $ chars
            char '"'
            return $ String x
  where chars = escaped <|> noneOf "\""
        escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
        escapedChar code replacement = char code >> return replacement
        codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"']
        replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"']

atom :: Parser Val
atom = do first <- letter <|> symbol
          rest <- many (letter <|> digit <|> symbol)
          let atom = first : rest
          return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            otherwise -> Atom atom

number :: Parser Val
-- number = do
  -- n <- many1 digit
  -- return (Number . read $ n)
-- number = many1 digit >>= return . Number . read
number = liftM (Number . read) $ many1 digit


expr :: Parser Val
expr = atom <|> string <|> number