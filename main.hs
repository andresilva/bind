module Main where
import Control.Monad
import Numeric
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec (Parser, (<|>), char, choice,
                                      digit, endBy, hexDigit, letter, many, many1,
                                      noneOf, octDigit, oneOf, parse, sepBy, skipMany1,
                                      space, try)

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
         | Float Float
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
number = liftM Number $ decimal <|> octal <|> hex
  where decimal = liftM read $ many1 digit
        hex = liftM (fst . head . readHex) $ try (P.string "#x") >> many1 hexDigit
        octal = liftM (fst . head . readOct) $ try (P.string "#o") >> many1 octDigit

float :: Parser Val
float = do
  integer <- many1 digit
  _ <- char '.'
  fractional <- many1 digit
  let float = integer ++ "." ++ fractional
  return $ Float $ fst $ readFloat float !! 0

list :: Parser Val
list = liftM List $ sepBy expr spaces

dottedList :: Parser Val
dottedList = do
  head <- endBy expr spaces
  tail <- char '.' >> spaces >> expr
  return $ DottedList head tail

quoted :: Parser Val
quoted = do
  char '\''
  x <- expr
  return $ List [Atom "quote", x]

lists :: Parser Val
lists = do
  char '('
  list <- try list <|> dottedList
  char ')'
  return list

expr :: Parser Val
expr = foldl1 (<|>) parsers
  where parsers = [try float, number, atom, string, quoted, lists]
