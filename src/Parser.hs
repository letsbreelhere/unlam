module Parser where

import Control.Applicative (Alternative(..))
import Control.Arrow (first)
import Control.Monad (void, guard)
import Data.Foldable (asum)
import Types

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

parse' :: Parser a -> String -> Maybe a
parse' p s = do (a, s') <- parse p s
                guard (null s')
                pure a

instance Functor Parser where
  fmap f p = Parser $ \s -> first f <$> parse p s

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  pf <*> px = Parser $ \s -> do (f, s') <- parse pf s
                                parse (f <$> px) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  p <|> p' = Parser $ \s -> parse p s <|> parse p' s

optional :: Parser a -> Parser (Maybe a)
optional p = fmap Just p <|> pure Nothing

char :: Char -> Parser Char
char c = Parser $ \s -> do (x:xs) <- pure s
                           guard (x == c)
                           pure (x, xs)

oneOf :: (Functor t, Foldable t) => t Char -> Parser Char
oneOf = asum . fmap char

skip :: (Functor f) => f a -> f ()
skip = void

spaces :: Parser ()
spaces = skip . many $ oneOf [' ', '\n', '\t']

token :: Parser a -> Parser a
token p = p <* spaces

lam :: Parser Lam'
lam = spaces *> (parseVar <|> parseAbstr <|> parseApp)

parseVar :: Parser Lam'
parseVar = fmap var (symbol '$' *> parseVarName)

parseVarName :: Parser String
parseVarName = token $ some . oneOf $ ['a'..'z'] ++ ['A'..'Z']

symbol :: Char -> Parser Char
symbol = token . char

parseAbstr :: Parser Lam'
parseAbstr = abstr <$> (symbol '^' *> parseVarName) <*> (symbol '.' *> lam)

parseApp :: Parser Lam'
parseApp = symbol '`' *> (app <$> lam <*> lam)
