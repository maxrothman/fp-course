{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Course.StateT where

import Course.Core
import Course.ExactlyOne
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.State
import qualified Data.Set as S
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- It'd be nice to be able to show the types in this, but that's probably hard because of polymorphism
instance Show a => Show (StateT s f a) where
  show _ = "StateT"

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<$>) func (StateT state) = StateT ((<$>) applyFst . state)
    where applyFst (x, y) = (func x, y)
-- Oooooh, This isn't (<$>) (applyFst . state), it's ((<$>) applyFst) . state


-- | Implement the `Applicative` instance for @StateT s f@ given a @Monad f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
--
-- >>> runStateT (StateT (\s -> ((+2), s P.++ [1]) :. ((+3), s P.++ [1]) :. Nil) <*> (StateT (\s -> (2, s P.++ [2]) :. Nil))) [0]
-- [(4,[0,1,2]),(5,[0,1,2])]
instance Monad f => Applicative (StateT s f) where
  pure :: a -> StateT s f a
  pure a = StateT $ pure . (a,)

  (<*>) ::
   StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  (<*>) (StateT mf) (StateT ma) = StateT $ \s -> do
      (f, s')  <- mf s
      (a, s'') <- ma s'
      pure (f a, s'')

-- | Implement the `Monad` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
--
-- >>> let modify f = StateT (\s -> pure ((), f s)) in runStateT (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad f => Monad (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  (=<<) f (StateT state) = StateT $ \s -> do
    (a, s') <- state s
    runStateT (f a) s'
-- REVIEW: Their solution is point-free for s, is theirs desirable or too clever?

-- | A `State'` is `StateT` specialised to the `ExactlyOne` functor.
type State' s a =
  StateT s ExactlyOne a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- ExactlyOne  ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' f = StateT $ ExactlyOne . f

-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT st) = runExactlyOne . st

-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT st) = (<$>) snd . st
-- REVIEW: I feel like I should be able to eta-reduce `st` out, but for that I'd need to replace (.)
-- with one of this type:
-- (c -> d) -> (a -> b -> c) -> a -> b -> d
-- and I couldn't find any such function on hoogle

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' st = runExactlyOne . execT st

-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT st) = (<$>) fst . st

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' st = runExactlyOne . evalT st

-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Applicative f =>
  StateT s f s
getT = StateT $ \s -> pure (s, s)

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Applicative f =>
  s
  -> StateT s f ()
putT = StateT . const . pure . ((),)

-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> \xs -> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' l = eval' (filtering pred l) S.empty
  where
    pred itm = do
      set <- getT
      let notMember = S.notMember itm set   -- REVIEW: this isn't going to make S.notMember get evaluated twice, is it?
      if notMember
        then putT $ S.insert itm set
        else pure ()
      pure notMember
-- REVIEW: theirs is a 1-liner, but uses more Control.Arrow stuff. Is it overly clever?

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  forall a. (Ord a, Num a, Show a) =>
  List a
  -> Optional (List a)
distinctF l = evalT (filtering pred l) S.empty
  where
    pred :: a -> StateT (S.Set a) Optional Bool
    pred itm =
      if itm > 100
        then StateT $ const Empty
        else do
          set <- getT
          let notMember = S.notMember itm set
          if notMember
            then putT $ S.insert itm set
            else pure ()
          pure notMember
-- REVIEW: theirs is much shorter, and doesn't use any Control.Arrow. Is this overly verbose?

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  (<$>) func (OptionalT fopt) = OptionalT $ (<$>) func <$> fopt

-- | Implement the `Applicative` instance for `OptionalT f` given a Monad f.
--
-- /Tip:/ Use `onFull` to help implement (<*>).
--
-- >>> runOptionalT $ OptionalT Nil <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- []
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT Nil
-- []
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Empty :. Nil)
-- [Empty,Empty]
--
-- >>> runOptionalT $ OptionalT (Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Empty :. Nil) <*> OptionalT (Full 1 :. Full 2 :. Nil)
-- [Full 2,Full 3,Empty]
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Monad f => Applicative (OptionalT f) where
  pure = OptionalT . pure . Full

  OptionalT ffunc <*> OptionalT fa = OptionalT $ do
    maybeFunc <- ffunc
    case maybeFunc of
      Empty     -> pure Empty
      Full func -> do
        a <- fa
        pure $ func <$> a
    -- REVIEW:
    -- Above, you suggested solutions like these over more point-free ones like theirs
    -- (OptionalT f <*> OptionalT a = OptionalT (lift2 (<*>) f a)). Does this fall into the same
    -- category? I'm still trying to build my intuition around when it makes sense to go point-free
    -- and when it doesn't.
    --
    -- Also, their version fails tests 5-7
    -- (https://github.com/maxrothman/fp-course/blob/e20fcd61979b76778836dd5277181937ad6facc3/test/Course/StateTTest.hs#L133)
    -- Mine did too when I had `a <- fa` in the top do. Moving it to the bottom do fixed the issue.
    --
    -- Also not really sure what the suggestion to use onFull was about, they don't use it either.


-- | Implement the `Monad` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Monad (OptionalT f) where
  func =<< OptionalT fa = OptionalT $ do
    maybeA <- fa
    case maybeA of
      Empty -> pure Empty
      Full a -> runOptionalT . func $ a
      -- REVIEW: un-OptionalT'ing this to just re-OptionalT it again seems dumb. There's no
      -- straightforward way around it, right? And I can assume the extra work will be optimized away?

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
 func <$> Logger l a = Logger l (func a)

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
--
-- >>> Logger (listh [1,2]) (+7) <*> Logger (listh [3,4]) 3
-- Logger [1,2,3,4] 10
instance Applicative (Logger l) where
  pure = Logger Nil
  Logger l1 f <*> Logger l2 a = Logger (l1 ++ l2) (f a)

-- | Implement the `Monad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Monad (Logger l) where
  func =<< Logger l a =
    let Logger l' a' = func a in
      Logger (l ++ l') a'

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 l a = Logger (l :. Nil) a

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG :: forall a.
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG l = runOptionalT $ evalT (filtering func l) S.empty
  where
    func :: a -> StateT (S.Set a) (OptionalT (Logger Chars)) Bool
    func itm =
      if | itm > 100 -> StateT . const . OptionalT $ log1 (fromString $ "aborting > 100: " P.++ show itm) Empty
         | even itm -> ifMember itm
            (\i s -> logEven i $ Full (False, s))
            (\i s -> logEven i $ Full (True, S.insert i s))
         | otherwise -> ifMember itm
            (\_ _ -> pure False)
            (\_ _ -> pure True)
      
    logEven itm val = StateT . const . OptionalT $ log1 (fromString  $ "even number: " P.++ show itm) val
    ifMember itm ifTrue ifFalse = do
      theSet <- getT
      if S.notMember itm theSet
        then ifFalse itm theSet
        else ifTrue itm theSet
    
    -- REVIEW: there's got to be an easier way to do this

onFull ::
  Applicative f =>
  (t -> f (Optional a))
  -> Optional t
  -> f (Optional a)
onFull g o =
  case o of
    Empty ->
      pure Empty
    Full a ->
      g a
