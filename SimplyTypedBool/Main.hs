module Main where

import Text.Megaparsec
import Text.Megaparsec.Char hiding (State)
import Text.Parser.Combinators (chainl1)
import qualified Control.Monad.State.Lazy as S
import Data.Void
import Data.List

iff p c a = if p then c else a

data Type = Bool
          | To Type Type
          deriving Eq

instance Show Type where
         show Bool = "Bool"
         show (To (To f1 a1) a2) = "(" ++ show f1 ++ " -> " ++ show a1 ++ ")" ++ " -> " ++ show a2
         show (To f a) = (unwords $ intersperse "->" $ map show [f, a])

data Term = Var SourcePos Int
          | Lam SourcePos String Type Term
          | App SourcePos Term Term
          | If SourcePos Term Term Term
          | T SourcePos
          | F SourcePos

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (a : _) = Just a
nth n (_ : rest) = nth (n - 1) rest

merge :: [a] -> [a] -> [a]
merge [] l = l
merge (x : xs) ys = x : merge ys xs

showTerm :: [String] -> Term -> Maybe String
showTerm env (Var _ n) = nth n env
showTerm env (Lam _ v t b) = showTerm (v : env) b >>= (\b -> return $ unwords ["lambda", v, ":", show t, ".", b])
showTerm env (App _ (App _ f a1) a2) = (sequence $ map (showTerm env) [f, a1, a2]) >>= (return . unwords)
showTerm env (App _ f a) = (sequence $ map (showTerm env) [f, a]) >>= (\fa -> return $ "(" ++ unwords fa ++ ")")
showTerm env (If _ p c a) = (sequence $ map (showTerm env) [p, c, a]) >>= (return . unwords . merge ["if", "then", "else"])
showTerm env (T _) = Just "true"
showTerm env (F _) = Just "false"

shift :: Int -> Int -> Term -> Term
shift a c (Var pos n) = iff (n < c) (Var pos n) (Var pos $ n + a)
shift a c (Lam pos v t b) = Lam pos v t $ shift a (c + 1) b
shift a c (App pos t1 t2) = App pos (shift a c t1) (shift a c t2)
shift a c (If pos p cons alt) = If pos (shift a c p) (shift a c cons) (shift a c alt)
shift _ _ t = t

sub :: Int -> Term -> Term -> Term
sub v t (Var pos n) = iff (v == n) t (Var pos n)
sub v t (Lam pos s ty b) = Lam pos s ty $ sub (v + 1) (shift 1 0 t) b
sub v t (App pos f a) = App pos (sub v t f) (sub v t a)
sub v t (If pos p c a) = If pos (sub v t p) (sub v t c) (sub v t a)
sub _ _ t = t

eval :: Term -> Term
eval (App _ (Lam _ _ _ b) a@(Lam _ _ _ _)) = eval $ shift (-1) 0 $ sub 0 (shift 1 0 a) b
eval (App _ (Lam _ _ _ b) (T pos)) = eval $ shift (-1) 0 $ sub 0 (T pos) b
eval (App _ (Lam _ _ _ b) (F pos)) = eval $ shift (-1) 0 $ sub 0 (F pos) b
eval (App pos f@(Lam _ _ _ _) a) = eval $ App pos f $ eval a
eval (App pos f a) = eval $ App pos (eval f) a
eval (If _ (T _) c a) = eval c
eval (If _ (F _) c a) = eval a
eval (If pos p c a) = eval $ If pos (eval p) c a
eval t = t

pos :: String -> [String] -> Maybe Int
pos = walk 0
      where walk _ _ [] = Nothing
            walk n a (b : rest) = iff (a == b) (Just n) (walk (n + 1) a rest)

parseTerms :: String -> Either (ParseErrorBundle String Void) [Term]
parseTerms input = S.evalState (runParserT terms "" input) []

terms :: ParsecT Void String (S.State [String]) [Term]
terms = sepEndBy term (space *> char ';' <* space)
term = space >> (expr)
parens = char '(' *> expr <* char ')'
expr = app <|> val
app = chainl1 (try (space >> (parens <|> val))) (getSourcePos >>= (pure . App))
val = lam <|> ifs <|> t <|> f <|> var
typ = space >> ((try tapp) <|> tval)
tapp = To <$> (tval <* space <* string "->")
          <*> typ
tval = (string "Bool" >> pure Bool) <|> tparens
tparens = char '(' *> typ <* char ')'
lam = do
       var <- string "lambda" >> space1 *> (some $ noneOf " \n\t\r.:()") <* space
       t <- char ':' *> typ <* space <* char '.'
       s <- S.get
       pos <- getSourcePos
       (Lam pos var t <$> (S.modify (var :) >> term)) <* S.put s
var = do
      var <- some $ noneOf " \n\t\r.();"
      iff (var == "then" || var == "else") (fail "Cannot parse var from keyword") (pure ())
      env <- S.gets (pos var)
      pos <- getSourcePos
      maybe (fail $ "Cannot parse free variable " ++ var) (pure . Var pos) env
ifs = If <$> getSourcePos
         <*> (string "if" >> space1 >> term <* space1)
         <*> (string "then" >> space1 >> term <* space1)
         <*> (string "else" >> space1 >> term)
t = string "true" >> getSourcePos >>= (pure . T)
f = string "false" >> getSourcePos >>= (pure . F)

checkType :: [Type] -> Term -> Either String Type
checkType env (Var pos n) = maybe (Left $ "Undefined variable at " ++ sourcePosPretty pos) (Right) $ nth n env
checkType env (Lam pos v t b) = checkType (t : env) b >>= (return . To t)
checkType env (App pos f a) = do
                              f' <- checkType env f
                              a' <- checkType env a
                              case f' of
                                   To b c | b == a' -> Right c
                                   _ -> Left $ "Parameter type mismatch at " ++ sourcePosPretty pos
checkType env (If pos p c a) = do
                               p <- checkType env p
                               c <- checkType env c
                               a <- checkType env a
                               case (p, c == a) of
                                    (Bool, True) -> Right c
                                    (_, True) -> Left $ "If predicate is not bool at " ++ sourcePosPretty pos
                                    (_, False) -> Left $ "If path types don't match at " ++ sourcePosPretty pos
checkType env (T _) = Right Bool
checkType env (F _) = Right Bool

main :: IO [()]
main = do
       getContents >>= (sequence
                        . either ((: []) . putStrLn)
                                 (map (putStrLn . (maybe "Term contains free variable" id) . showTerm [] . eval))
                        . (>>= (\terms -> (sequence $ map (checkType []) terms) >> return terms))
                        . either (Left . errorBundlePretty) Right
                        . parseTerms)
