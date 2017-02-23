module Types where

import Data.Functor.Foldable (Fix(..), cata)

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

data App f =
  App f
      f
   deriving (Functor, Foldable, Traversable)

data Lam f
  = Abs String
        f
  | Var String
        Bool
  deriving (Functor, Foldable, Traversable)

data Ski f
  = S
  | K
  | I
  deriving (Functor, Foldable, Traversable)

instance Show (Ski f) where
  show S = "s"
  show K = "k"
  show I = "i"

infixl 9 :+:

data (f :+: g) a
  = InL (f a)
  | InR (g a)
  deriving (Functor, Foldable, Traversable)

type Lam' = Fix (App :+: Lam)
type SkiTree = Fix (App :+: Ski)
type LamWithSki = Fix ((App :+: Ski) :+: Lam)

pattern l :<@> r = InL (InL (App l r))
pattern Lam x = InR x
pattern Ski x = InL (InR x)

app :: Lam' -> Lam' -> Lam'
app l r = Fix (InL $ App l r)

var :: String -> Lam'
var s = Fix (InR $ Var s False)

abstr :: String -> Lam' -> Lam'
abstr v e = Fix (InR $ Abs v e)

mkLam :: Lam LamWithSki -> LamWithSki
mkLam = Fix . InR

mkSki :: Ski LamWithSki -> LamWithSki
mkSki = Fix . InL . InR

mkAbs :: String -> LamWithSki -> LamWithSki
mkAbs v e = mkLam $ Abs v e

mkVar :: String -> Bool -> LamWithSki
mkVar s mark = mkLam (Var s mark)

(<@>) :: LamWithSki -> LamWithSki -> LamWithSki
l <@> r = Fix . InL . InL $ l `App` r

showSkiTree :: SkiTree -> String
showSkiTree =
  cata $
  \case
    InL (App l r) -> '`' : l ++ r
    InR s -> show s

showLamWithSki :: LamWithSki -> String
showLamWithSki =
  cata $
  \case
    l :<@> r -> '`' : l ++ r
    Lam (Var v _) -> '$' : v
    Lam (Abs v e) -> "^" ++ v ++ "." ++ e
    Ski s -> show s
