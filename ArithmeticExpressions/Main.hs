module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import System.Environment

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

isValue :: Term -> Bool
isValue T = True
isValue F = True
isValue t = isNumericValue t

step :: Term -> Maybe Term
step (If T c _) = Just c
step (If F _ a) = Just a
step (If p c a) = step p >>= (\p -> return $ If p c a)
step (S t) = step t >>= (return . S)
step (P Z) = Just Z
step (P (S t)) = if isNumericValue t then Just t else Nothing
step (P t) = step t >>= (return . P)
step (IsZ Z) = Just T
step (IsZ (S _)) = Just F
step (IsZ t) = step t >>= (return . IsZ)
step _ = Nothing

eval :: Term -> Term
eval t = maybe t eval $ step t

numericTermToInt :: Term -> Int
numericTermToInt Z = 0
numericTermToInt (S t) = 1 + (numericTermToInt t)

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
