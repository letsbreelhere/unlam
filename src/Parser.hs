module Parser where

import Control.Applicative (Alternative(..))
import Control.Arrow (first)
import Control.Monad (guard)
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

char :: Char -> Parser Char
char c = Parser $ \s -> do (x:xs) <- pure s
                           guard (x == c)
                           pure (x, xs)

oneOf :: (Functor t, Foldable t) => t Char -> Parser Char
oneOf = foldr (<|>) empty . fmap char

spaces :: Parser ()
spaces = many (oneOf (" \n\t" :: String)) *> pure ()

token :: Parser a -> Parser a
token p = p <* spaces

lam :: Parser Lam'
lam = (var <$> parseVar) <|> parseAbstr <|> parseApp

parseVar :: Parser Char
parseVar = token . oneOf $ ['a'..'z'] ++ ['A'..'Z']

symbol :: Char -> Parser Char
symbol = token . char

parseAbstr :: Parser Lam'
parseAbstr = abstr <$> (symbol '^' *> parseVar) <*> (symbol '.' *> lam)

parseApp :: Parser Lam'
parseApp = symbol '`' *> (app <$> lam <*> lam)
