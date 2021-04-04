module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Term = T
          | F
          | If Term Term Term
          | Z
          | S Term
          | P Term
          | IsZ Term
          deriving Show

isNumericValue :: Term -> Bool
isNumericValue Z = True
isNumericValue (S t) = isNumericValue t
isNumericValue (P t) = isNumericValue t
isNumericValue _ = False

eval :: Term -> Term
eval (If T c _) = eval c
eval (If F _ a) = eval a
eval (If p c a) = eval (If (eval p) c a)
eval (S t) = S (eval t)
eval (P Z) = Z
eval (P (S t)) = if isNumericValue t then eval t else P (S t)
eval (P t) = eval $ P $ eval t
eval (IsZ Z) = T
eval (IsZ (S _)) = F
eval (IsZ t) = eval $ IsZ $ eval t
eval t = t

numericTermToInt :: Term -> Int
numericTermToInt Z = 0
numericTermToInt (S t) = (numericTermToInt t) + 1;

termToString :: Term -> String
termToString T = "true"
termToString F = "false"
termToString t
             | isNumericValue t = show $ numericTermToInt t
             | otherwise = show t

parseTerm :: String -> Either (ParseErrorBundle String Void) Term
parseTerm = parse term ""

parseTerms :: String -> Either (ParseErrorBundle String Void) [Term]
parseTerms = parse terms ""

terms = sepEndBy term (space *> char ';' <* space)
term = space >> (parens <|> expr)
parens = (char '(' >> space) *> expr <* (space >> char ')')
expr = space >> (value <|> fun <|> ifs)
value = true <|> false <|> zero
true = string "true" >> pure T
false = string "false" >> pure F
zero = char '0' >> pure Z
fun = succf <|> predf <|> iszerof
succf = string "succ" >> (S <$> term)
predf = string "pred" >> (P <$> term)
iszerof = string "iszero" >> (IsZ <$> term)
ifs = do
      string "if"
      space1
      pred <- term
      space1
      string "then"
      space1
      cons <- term
      space1
      string "else"
      space1
      alt <- term
      return $ If pred cons alt

main :: IO ()
main = do
       stdin <- getContents
       sequence $ either ((: []) . putStrLn . errorBundlePretty)
                         (map (putStrLn . termToString . eval))
                         (parseTerms stdin)
       return ()
