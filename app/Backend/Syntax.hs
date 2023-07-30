module Backend.Syntax where

import Backend.Language
import Control.Monad (void)
import Control.Applicative (Alternative)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = read <$> some digitChar

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

operatorTable :: [[Operator Parser LambdaExpr]]
operatorTable = [[InfixL (Add <$ symbol "+")],
                 [InfixL (App <$ space)]]

term :: Parser LambdaExpr
term = try (chainl1 simpleTerm (pure App)) <|> simpleTerm
  where
    simpleTerm = parens expr <|> (Const <$> integer) <|> (Var <$> some letterChar)

chainl1 :: (Alternative m, Monad m) =>
     m b -> m (b -> b -> b) -> m b
chainl1 p op = p >>= rest
    where
        rest x = (do f <- op
                     y <- p
                     rest (f x y)) <|> return x

expr :: Parser LambdaExpr
expr = makeExprParser term operatorTable

lambdaExpr :: Parser LambdaExpr
lambdaExpr = try (makeFun =<< many (try lam1Expr)) <|> atom

atom :: Parser LambdaExpr
atom = try expr
   <|> Var <$> some letterChar

lam1Expr :: Parser [String]
lam1Expr = do
  void $ string "\\"
  var <- some (some letterChar <* space)
  void $ string "->" <* space
  return var

makeFun :: [[String]] -> Parser LambdaExpr
makeFun [] = fail "No lambda expression"
makeFun xs = do
  body <- lambdaExpr
  return $ foldr (\v acc -> Lam v acc) body (concat xs)

program :: Parser Program
program = do
    namedParts <- many (try namedPart <* semicolon)
    commandPart <- command
    return $ foldr (\(name, expr) prog -> Named name expr prog) commandPart namedParts
  where
    semicolon = skipMany spaceChar *> char ';' <* skipMany spaceChar

namedPart :: Parser (String, LambdaExpr)
namedPart = do
    name <- some letterChar <* skipMany spaceChar
    void $ char '=' <* skipMany spaceChar
    expr <- lambdaExpr
    return (name, expr)

command :: Parser Program
command = Command <$> lambdaExpr <* eof

test :: String -> Program
test x = case (parse program "" x) of
              Left err -> error $ show err
              Right p -> p

