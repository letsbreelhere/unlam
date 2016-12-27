{-# LANGUAGE LambdaCase #-}

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

import Control.Monad.Trans (liftIO)
import System.Console.Haskeline (getInputLine)
import qualified System.Console.Haskeline as Haskeline
import Types
import Parser
import Control.Monad (replicateM)

removePoint :: Char -> LamWithSki -> LamWithSki
removePoint v =
    cata $
    \case
        e :<@> e' -> mkSki S <@> e <@> e'
        Lam l@(Var v' _)
                    | v == v' -> mkSki I
                    | otherwise -> mkSki K <@> mkLam l
        Lam (Abs v' e)
          | v == v' -> error "Failure: encountered nested identical variables (this should be impossible with alpha-conversion)"
          | otherwise -> mkSki K <@> removePoint v' e
        Ski s -> mkSki K <@> mkSki s

pointfree :: LamWithSki -> LamWithSki
pointfree (Fix (Lam (Abs v e))) = removePoint v e
pointfree lws = lws

-- Convert vars named c to c' and mark them.
mark :: Char -> Char -> LamWithSki -> LamWithSki
mark c c' = cata $ \case
  Lam (Var v False) | v == c -> mkVar c' True
  expr -> Fix expr

inject :: Lam' -> LamWithSki
inject = hmap (mapR InL)

alphaConversion :: LamWithSki -> (Int, LamWithSki)
alphaConversion = cata $ \case
  (k, e) :<@> (m, e') -> (max k m, e <@> e')
  Ski s -> (0, mkSki (fmap snd s))
  Lam (Abs v (n, e)) -> do
    let n' = n + 1
    let v' = ['a'..] !! n'
    let e' = mark v v' e
    (n', mkLam $ Abs v' e')
  Lam (Var v reduced) -> (0, mkVar v reduced)

main :: IO ()
main = Haskeline.runInputT Haskeline.defaultSettings loop
  where
    loop = do
      input <- getInputLine "> "
      let term = parse' lam =<< input
      liftIO . putStrLn $
        maybe "Failed to parse." (display . transform . inject) term
      loop

    transform :: LamWithSki -> LamWithSki
    transform = pointfree . snd . alphaConversion

    display :: LamWithSki -> String
    display = ("=> " ++) . showLamWithSki
