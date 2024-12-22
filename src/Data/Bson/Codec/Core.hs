module Data.Bson.Codec.Core (Codec(..)) where

{-

I've been facinated by the bidirectional serialization in the codec
and tomland packages.  Can I do that for Bson?  Let's try!

 -}

import Control.Applicative (Alternative, empty, (<|>))
import Data.Kind (Type)

import Data.Profunctor


data Codec (r :: Type -> Type) (w :: Type -> Type) x a = Codec
  { codecReader :: r a
  , codecWriter :: x -> w a
  }

instance (Functor r, Functor w) => Functor (Codec r w x) where
  fmap :: (a -> b) -> Codec r w x a -> Codec r w x b
  fmap f (Codec r w) = Codec (fmap f r) (fmap f . w)

instance (Applicative r, Applicative w) => Applicative (Codec r w x) where
  pure :: a -> Codec r w x a
  pure a = Codec (pure a) (const $ pure a)

  (<*>) :: Codec r w x (a -> b) -> Codec r w x a -> Codec r w x b
  Codec fr fw <*> Codec ar aw = Codec (fr <*> ar) (\x -> fw x <*> aw x)

instance (Alternative r, Alternative w) => Alternative (Codec r w x) where
  empty :: Codec r w x a
  empty = Codec empty (const empty)

  (<|>) :: Codec r w x a -> Codec r w x a -> Codec r w x a
  Codec r w <|> Codec r' w' = Codec (r <|> r') (\x -> w x <|> w' x)

instance (Functor r, Functor w) => Profunctor (Codec r w) where
  dimap :: (y -> x) -> (a -> b) -> Codec r w x a -> Codec r w y b
  dimap from to (Codec r w) = Codec (fmap to r) (fmap to . w . from)

  lmap :: (y -> x) -> Codec r w x a -> Codec r w y a
  lmap from (Codec r w) = Codec r (w . from)

  rmap :: (a -> b) -> Codec r w x a -> Codec r w x b
  rmap = fmap

instance (Monad r, Monad w) => Monad (Codec r w x) where
  (>>=) :: Codec r w x a -> (a -> Codec r w x b) -> Codec r w x b
  Codec r w >>= f = Codec (r >>= codecReader . f) (\x -> w x >>= \a -> codecWriter (f a) x)
