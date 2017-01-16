{-# LANGUAGE RankNTypes, TypeOperators #-}

module Control.Natural where

import Types
import Data.Functor.Foldable (Fix(..))

infixl 0 ~>
type f ~> g = forall a. f a -> g a

(<+>) :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
(l <+> _) (InL fa) = l fa
(_ <+> r) (InR gb) = r gb

swap :: (f :+: g) ~> (g :+: f)
swap = InR <+> InL

fixMap
  :: Functor g
  => (f ~> g) -> Fix f -> Fix g
fixMap f = Fix . fmap (fixMap f) . f . unFix

mapRight :: (r ~> r') -> (l :+: r ~> l :+: r')
mapRight natl = InL <+> (InR . natl)

mapLeft :: (l ~> l') -> (l :+: r ~> l' :+: r)
mapLeft natl = swap . mapRight natl . swap
