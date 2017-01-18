{-# LANGUAGE LambdaCase, RankNTypes, TypeOperators, DeriveFunctor
  #-}

module Control.Natural where

import Types
import Data.Functor.Foldable (Fix(..), cata)

newtype C f g a = C
  { unC :: f (g a)
  } deriving (Functor)

infixl 0 ~>

type f ~> g = forall a. f a -> g a

(<+>) :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
(l <+> _) (InL fa) = l fa
(_ <+> r) (InR gb) = r gb

swap :: f :+: g ~> g :+: f
swap = InR <+> InL

fixMap
  :: Functor g
  => (f ~> g) -> Fix f -> Fix g
fixMap f = Fix . fmap (fixMap f) . f . unFix

commuteLeft :: ((l :+: r) a -> c) -> (r :+: l) a -> c
commuteLeft = (. swap)

commuteRight :: (c -> (l :+: r) a) -> c -> (r :+: l) a
commuteRight = (swap .)

mapRight :: (r ~> r') -> (l :+: r ~> l :+: r')
mapRight natl = InL <+> (InR . natl)

mapLeft :: (l ~> l') -> (l :+: r ~> l' :+: r)
mapLeft = commuteRight . commuteLeft . mapRight

forgetLeft :: (l :+: r) ~> C Maybe r
forgetLeft = C . (const Nothing <+> Just)

forgetRight :: (l :+: r) ~> C Maybe l
forgetRight = commuteLeft forgetLeft

extractMaybe
  :: Traversable f
  => Fix (C Maybe f) -> Maybe (Fix f)
extractMaybe = cata (maybe Nothing (fmap Fix . sequenceA) . unC)

extractLeft
  :: Traversable l
  => Fix (l :+: r) -> Maybe (Fix l)
extractLeft = extractMaybe . fixMap forgetRight

extractRight
  :: Traversable r
  => Fix (l :+: r) -> Maybe (Fix r)
extractRight = extractMaybe . fixMap forgetLeft
