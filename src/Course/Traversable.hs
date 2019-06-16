{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose
import Debug.Trace

-- | All instances of the `Traversable` type-class must satisfy three laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable List where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> List a
    -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> ExactlyOne a
    -> f (ExactlyOne b)
  traverse func (ExactlyOne a) = ExactlyOne <$> func a
  -- REVIEW: I know this eta-reduction is wrong, but I don't know why.
  -- traverse func = ExactlyOne <$> func . runExactlyOne

instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)
  traverse func oa = case oa of
    Empty  -> pure Empty
    Full a -> Full <$> func a

  -- REVIEW: there's no answer key for this section, so I can't compare. Is there a clever way of
  -- doing this that doesn't require inspecting the value of the Optional?

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative f, Traversable t) =>
  t (f a)
  -> f (t a)
sequenceA = traverse id

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
-- Implement the traverse function for a Traversable instance for Compose
  traverse :: Applicative t => (a -> t b) -> Compose f g a -> t (Compose f g b)
  traverse func ca = (traverse . traverse) func ca
  -- traverse func (Compose fga) = ((<$>) . (<$>)) Compose func $ fga
  -- why doesn't this work? In ghci:
  -- >> :t func
  -- func :: Num a => a -> Optional a
  -- >> :t ((<$>) . (<$>)) Compose func $ ((1 :. Nil) :. Nil)
  -- ((<$>) . (<$>)) Compose func $ ((1 :. Nil) :. Nil)
  
  -- (<$>) . (<$>)
  -- (f c -> f d) -> g (f c) -> g (f d)  .  (c -> d) -> f c -> f d
  -- b               c                      a           b
  -- (c -> d) -> g (f c) -> g (f d)

  -- Compose :: fga -> Compose fga
  -- func :: a -> t b
  -- (fga -> Compose (f (g a))) -> (a -> t b) = a -> t (Compose f g a)

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
-- Implement the (<$>) function for a Functor instance for Product
  (<$>) =
    error "todo: Course.Traversable (<$>)#instance (Product f g)"

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
-- Implement the traverse function for a Traversable instance for Product
  traverse =
    error "todo: Course.Traversable traverse#instance (Product f g)"

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
-- Implement the (<$>) function for a Functor instance for Coproduct
  (<$>) =
    error "todo: Course.Traversable (<$>)#instance (Coproduct f g)"

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
-- Implement the traverse function for a Traversable instance for Coproduct
  traverse =
    error "todo: Course.Traversable traverse#instance (Coproduct f g)"
