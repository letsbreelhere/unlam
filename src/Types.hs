{-# LANGUAGE DeriveFunctor, FlexibleInstances, RankNTypes #-}

module Types where

data App f = App f f
  deriving (Functor)

data Lam f = Abs Char f
           | Var Char
           deriving (Functor)

data Ski f = S
           | K
           | I
           deriving (Functor)

data Fix f = Fix { unFix :: f (Fix f) }

type Natural f g = forall a. f a -> g a

-- Maps a natural transformation into `Fix`
hmap :: Functor g => Natural f g -> Fix f -> Fix g
hmap f = Fix . fmap (hmap f) . f . unFix

type Lam' = Fix (Sum App Lam)

instance Show (Ski f) where
  show S = "s"
  show K = "k"
  show I = "i"

data Sum f g a = InL (f a) | InR (g a)
  deriving (Functor)

mapR :: (r a -> r' a) -> Sum l r a -> Sum l r' a
mapR natl x = case x of
  InL l -> InL l
  InR r -> InR (natl r)

type LamWithSki = Fix (Sum App (Sum Lam Ski))

app :: Lam' -> Lam' -> Lam'
app l r = Fix (InL $ App l r)

var :: Char -> Lam'
var c =  Fix (InR $ Var c)

abstr :: Char -> Lam' -> Lam'
abstr v e = Fix (InR $ Abs v e)

mkLam :: Lam LamWithSki -> LamWithSki
mkLam = Fix . InR . InL

mkSki :: Ski LamWithSki -> LamWithSki
mkSki = Fix . InR . InR

mkAbs :: Char -> LamWithSki -> LamWithSki
mkAbs v e = mkLam $ Abs v e

(<@>) :: LamWithSki -> LamWithSki -> LamWithSki
l <@> r = Fix . InL $ l `App` r

showLamWithSki :: LamWithSki -> String
showLamWithSki (Fix (InL (App l r))) = '`' : showLamWithSki l ++ showLamWithSki r
showLamWithSki (Fix (InR x)) = case x of
  InL lambda -> case lambda of
               Var v -> [v]
               Abs v e -> '^' : v : '.' : showLamWithSki e
  InR ski -> show ski
