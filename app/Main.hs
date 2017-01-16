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

import Control.Natural
import Control.Monad.Trans (liftIO)
import Data.Functor.Foldable (Fix(..), cata)
import System.Console.Haskeline (getInputLine)
import qualified System.Console.Haskeline as Haskeline
import Types
import Parser

removePoint :: Char -> LamWithSki -> LamWithSki
removePoint v =
  cata $
  \case
    e :<@> e' -> mkSki S <@> e <@> e'
    Lam (Var v' _)
      | v == v' -> mkSki I
    Lam (Abs v' e)
      | v == v' ->
        error
          "Failure: encountered nested identical variables (this should be impossible with alpha-conversion)"
      | otherwise -> mkSki K <@> removePoint v' e
    l -> mkSki K <@> Fix l

pointfree :: LamWithSki -> LamWithSki
pointfree = cata $ \case
  Lam (Abs v e) -> removePoint v e
  lws -> Fix lws

-- Convert vars named c to c' and mark them.
mark :: Char -> Char -> LamWithSki -> LamWithSki
mark c c' = cata $ \case
  Lam (Var v False) | v == c -> mkVar c' True
  expr -> Fix expr

inject :: Lam' -> LamWithSki
inject = fixMap (mapRight InL)

-- `cata` here accumulates a fresh variable store which we use to convert each
-- lambda var we encounter from the bottom up.
alphaConvert :: LamWithSki -> LamWithSki
alphaConvert = snd . cata $ \case
  (k, e) :<@> (m, e') -> (max k m, e <@> e')
  Ski s -> (0, mkSki (fmap snd s))
  Lam (Abs v (n, e)) -> do
    let v' = ['a'..] !! n
    let e' = mark v v' e
    (n+1, mkLam $ Abs v' e')
  Lam (Var v reduced) -> (0, mkVar v reduced)

main :: IO ()
main = Haskeline.runInputT Haskeline.defaultSettings loop
  where
    loop = do
      inputMay <- getInputLine "> "
      case inputMay of
        Nothing -> return ()
        Just input -> do
          let term = parse' lam input
          liftIO . putStrLn $
            maybe ("Failed to parse." ++ show input) (display . transform . inject) term
          loop

    transform :: LamWithSki -> LamWithSki
    transform = pointfree . alphaConvert

    display :: LamWithSki -> String
    display = ("=> " ++) . showLamWithSki
