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

import System.IO (hSetBuffering, stdout, BufferMode(..))
import Types
import Parser
import Control.Monad (replicateM)
import Debug.Trace (trace)

removePoint :: Char -> LamWithSki -> LamWithSki
removePoint v =
    cata $
    \case
        InL (App e e') -> mkSki S <@> e <@> e'
        InR (InL lambda) ->
            case lambda of
                Var v' _
                    | v == v' -> mkSki I
                    | otherwise -> mkSki K <@> mkLam lambda
                Abs v' e
                    | v == v' -> error "Failure: encountered nested identical variables. (Need alpha-conversion to avoid this)"
                    | otherwise -> mkSki K <@> removePoint v' e
        InR (InR s) -> mkSki K <@> mkSki s

pointfree :: LamWithSki -> LamWithSki
pointfree (Fix (InR (InL (Abs v e)))) = removePoint v e
pointfree lws = lws

-- Convert vars named c to c' and mark them.
mark :: Char -> Char -> LamWithSki -> LamWithSki
mark c c' = cata $ \case
  InR (InL (Var x False)) | c == x -> Fix (InR $ InL $ Var c' True)
  expr -> Fix expr

inject :: Lam' -> LamWithSki
inject = hmap (mapR InL)

produceFresh :: Int -> LamWithSki -> (Int, LamWithSki)
produceFresh _ = cata $ \case
  InL (App (k, e) (m, e')) -> (max k m, e <@> e')
  InR (InR s) -> (0, mkSki (fmap snd s))
  InR (InL lambda) ->
      case lambda of
        Abs v (n, e) -> do
          let n' = n + 1
          let v' = ['a'..] !! n'
          let e' = mark v v' e
          (n', mkLam $ Abs v' e')
        Var v reduced -> (0, mkVar v reduced)

main :: IO ()
main = hSetBuffering stdout NoBuffering >> loop
  where
    loop = do
        putStr "> "
        term <- parse' lam <$> getLine
        putStrLn $
            maybe
                "Failed to parse."
                (display . transform . inject)
                term
        loop

    transform :: LamWithSki -> LamWithSki
    transform lws = do
      let (n, intermediate) = produceFresh 0 lws
      trace ("Reduced: " ++ showLamWithSki intermediate) $ pointfree intermediate

    display :: LamWithSki -> String
    display = ("=> " ++) . showLamWithSki
