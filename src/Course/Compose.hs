{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  -- (a -> b) -> Compose f g a -> Compose f g b
  func <$> Compose fga = Compose $ ((<$>) . (<$>)) func fga

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
-- <*>:   f (a -> b) -> f a -> f b
-- cfunc: f (g (a -> b))
-- ca:    f (g a)
-- f (g a -> g b)
  (Compose cfunc) <*> (Compose ca) = Compose $ lift2 (<*>) cfunc ca

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
    (=<<) = error "impossible"
  -- func =<< Compose fga = Compose $ do
  --   a <- id =<< fga
  --   let (Compose r) = func a
  --   r

  -- REVIEW: mechanically, I can see that this is impossible because I can't get f (g a) out of
  -- Compose without pattern-matching, and thus I can't run func on what I get because func returns a
  -- Compose, not an f (g a). But is there some more fundamental reason?

