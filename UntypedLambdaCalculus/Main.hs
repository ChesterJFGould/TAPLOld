module Main where

import Text.Megaparsec
import Text.Megaparsec.Char hiding (State)
import Text.Parser.Combinators (chainl1)
import qualified Control.Monad.State.Lazy as S
import Data.Void
import Data.List

iff p c a = if p then c else a

data Term = Var Int
          | Lam String Term
          | App Term Term
          deriving Show

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (a : _) = Just a
nth n (_ : rest) = nth (n - 1) rest

showTerm :: Term -> Maybe String
showTerm = walk []
           where walk env (Var n) = nth n env
                 walk env (Lam v b) = (walk (v : env) b) >>= (\b -> return $ "lambda " ++ v ++ ". " ++ b)
                 walk env (App (App f a1) a2) = (sequence $ map (walk env) [f, a1, a2]) >>= (return . unwords)
                 walk env (App f a) = (sequence $ map (walk env) [f, a]) >>= (\l -> return $ "(" ++ unwords l ++ ")")

shift :: Int -> Int -> Term -> Term
shift a c (Var n) = iff (n < c) (Var n) (Var $ n + a)
shift a c (Lam v body) = Lam v $ shift a (c + 1) body
shift a c (App t1 t2) = App (shift a c t1) (shift a c t2)

sub :: Int -> Term -> Term -> Term
sub var term (Var n) = iff (var == n) term (Var n)
sub var term (Lam v body) = Lam v $ sub (var + 1) (shift 1 0 term) body
sub var term (App f a) = App (sub var term f) (sub var term a)

eval :: Term -> Term
eval (App (Lam _ body) a@(Lam _ _)) = eval $ shift (-1) 0 $ sub 0 (shift 1 0 a) body
eval (App f@(Lam _ _) a) = eval $ App f $ eval a
eval (App f a) = eval $ App (eval f) a
eval t = t

parseTerms :: String -> Either (ParseErrorBundle String Void) [Term]
parseTerms input = S.evalState (runParserT nterms "" input) []

pos :: String -> [String] -> Maybe Int
pos = walk 0
      where walk _ _ [] = Nothing
            walk n a (b : rest) = iff (a == b) (return n) (walk (n + 1) a rest)

nterms :: ParsecT Void String (S.State [String]) [Term]
nterms = sepEndBy nterm (space *> char ';' <* space)
nterm = space >> (expr <|> parens)
parens = char '(' *> expr <* char ')'
expr = napp <|> val
napp = chainl1 (space >> (parens <|> val)) (return App)
val = nlam <|> nvar
nlam = do
       var <- string "lambda" >> space1 *> (some $ noneOf " \n\t\r.()") <* char '.'
       s <- S.get
       (Lam var <$> (S.modify (var :) >> nterm)) <* S.put s
nvar = do
       var <- some $ noneOf " \n\t\r.();"
       pos <- S.gets (pos var)
       maybe (fail $ "Cannot parse free variable " ++ var) (pure . Var) pos

main :: IO [()]
main = do
       getContents >>= (sequence
                        . either ((: []) . putStrLn . errorBundlePretty)
                                 (map (putStrLn . (maybe "Term contains free variable" id) .showTerm . eval))
                        . parseTerms)
