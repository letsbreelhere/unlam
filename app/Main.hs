{-
A fun exercise: read lambda calculus expressions and convert them to
pointfree form in the SKI-calculus. Parsing (and output via Show) are written
using Unlambda conventions:
  * Lambda-abstraction is written "^v.E" (equivalent to Haskell "\v -> E")
  * Application is written "`lr" (equivalent to "l r")
Also, note that the conversion function gives priority innermost abstractions,
so e.g. "^x.^x.x" is equivalent to "^x.^y.y".

An example (computing the B combinator):

```
> ^f.^g.^x.`f`gx
=> ``s``s`ks``s``s`ks``s`kk`ks``s``s`ks``s`kk`kk``s`kki``s``s`ks``s``s`ks``s`kk`ks``s``s`ks``s`kk`kk`ki``s`kk`ki
```
-}

module Main where

import System.IO (hSetBuffering, stdout, BufferMode(..))
import Types
import Parser

removePoint :: Char -> LamWithSki -> LamWithSki
removePoint v (Fix (InL (App e e'))) = (mkSki S <@> removePoint v e) <@> removePoint v e'
removePoint v (Fix (InR (InL l))) = case l of
  Var v' | v == v'     -> mkSki I
         | otherwise   -> mkSki K <@> mkLam l
  Abs v' e | v == v'   -> error "Bad term! (this should be impossible)"
           | otherwise -> mkSki K <@> removePoint v' (removePoint v e)
removePoint _ (Fix (InR (InR s))) = mkSki K <@> mkSki s

pointfree :: LamWithSki -> LamWithSki
pointfree (Fix (InR (InL (Abs v e)))) = removePoint v e
pointfree lws = lws

main :: IO ()
main = hSetBuffering stdout NoBuffering >> loop
  where loop = do
          putStr "> "
          term <- parse' lam <$> getLine
          putStrLn $ maybe "Failed to parse." (("=> " ++) . showLamWithSki . pointfree . hmap (mapR InL)) term
          loop
