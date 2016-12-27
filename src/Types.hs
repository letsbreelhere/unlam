{-# LANGUAGE DeriveFunctor, FlexibleInstances, RankNTypes,
  TypeOperators, LambdaCase, PatternSynonyms #-}

module Types where

data App f =
    App f
        f
     deriving (Functor)

data Lam f
  = Abs Char
          f
  | Var Char Bool
  deriving (Functor)

data Ski f
    = S
    | K
    | I
    deriving (Functor)

data Fix f = Fix
    { unFix :: f (Fix f)
    }

cata
    :: Functor f
    => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

infixl 0 ~>
type f ~> g = forall a. f a -> g a

hmap
    :: Functor g
    => (f ~> g) -> Fix f -> Fix g
hmap f = Fix . fmap (hmap f) . f . unFix

type Lam' = Fix (App :+: Lam)

instance Show (Ski f) where
    show S = "s"
    show K = "k"
    show I = "i"

infixl 9 :+:

data (f :+: g) a
    = InL (f a)
    | InR (g a)
     deriving (Functor)

(<+>) :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
(h <+> k) sum =
    case sum of
        InL fa -> h fa
        InR gb -> k gb

mapR :: (r ~> r') -> (l :+: r ~> l :+: r')
mapR natl = InL <+> (InR . natl)

type LamWithSki = Fix (App :+: (Lam :+: Ski))

-- Pattern helpers for LamWithSki
pattern l :<@> r = InL (App l r)
pattern Lam x = InR (InL x)
pattern Ski x = InR (InR x)

app :: Lam' -> Lam' -> Lam'
app l r = Fix (InL $ App l r)

var :: Char -> Lam'
var c = Fix (InR $ Var c False)

abstr :: Char -> Lam' -> Lam'
abstr v e = Fix (InR $ Abs v e)

mkLam :: Lam LamWithSki -> LamWithSki
mkLam = Fix . InR . InL

mkSki :: Ski LamWithSki -> LamWithSki
mkSki = Fix . InR . InR

mkAbs :: Char -> LamWithSki -> LamWithSki
mkAbs v e = mkLam $ Abs v e

mkVar :: Char -> Bool -> LamWithSki
mkVar c mark = Fix (InR $ InL $ Var c mark)

(<@>) :: LamWithSki -> LamWithSki -> LamWithSki
l <@> r = Fix . InL $ l `App` r

showLamWithSki :: LamWithSki -> String
showLamWithSki =
    cata $
    \case
        l :<@> r -> '`' : l ++ r
        Lam (Var v _) -> [v]
        Lam (Abs v e) -> '^' : v : '.' : e
        Ski s -> show s
