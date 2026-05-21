-- | Core data type and combinators for logging actions.
-- |
-- | Port of @Colog.Core.Action@ from the Haskell @co-log-core@ library. The
-- | whole library is built on a single type:
-- |
-- | ```purescript
-- | newtype LogAction m msg = LogAction (msg -> m Unit)
-- | ```
-- |
-- | which is a `Semigroup`, `Monoid`, `Contravariant`, `Divide`, `Divisible`,
-- | `Decide`, and `Decidable` (the contravariant hierarchy from the
-- | `contravariant` package fits it exactly). It is *not* a `Functor`/`Comonad`
-- | (it is contravariant), so the comonad-style combinators are standalone
-- | functions, exactly as in the original.
module Colog.Core.Action
  ( LogAction(..)
  , unLogAction
  , logActionFlipped
  , foldActions
  , cmapM
  , cmapMaybe
  , cmapMaybeM
  , cfilter
  , cfilterM
  , cconst
  , divideM
  , chooseM
  , extract
  , extend
  , extendFlipped
  , duplicate
  , multiplicate
  , separate
  , hoistLogAction
  , (<&)
  , (&>)
  , (>$)
  , (>*<)
  , (>|<)
  , (=>>)
  , (<<=)
  , module Data.Functor.Contravariant
  , module Data.Divide
  , module Data.Divisible
  , module Data.Decide
  , module Data.Decidable
  ) where

import Prelude

import Data.Decidable (class Decidable, lose)
import Data.Decide (class Decide, choose, chosen)
import Data.Divide (class Divide, divide, divided)
import Data.Divisible (class Divisible, conquer)
import Data.Either (Either, either)
import Data.Foldable (class Foldable, fold, for_, traverse_)
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))

----------------------------------------------------------------------------
-- Core data type with instances
----------------------------------------------------------------------------

-- | Polymorphic, very general logging action.
-- |
-- | * `msg` is the input the logger consumes (e.g. `String` or a custom record).
-- | * `m` is the monad the logging happens in (e.g. `Effect`).
newtype LogAction m msg = LogAction (msg -> m Unit)

-- | Unwrap a `LogAction` to the underlying consumer function. Also available as
-- | the `<&` operator.
unLogAction :: forall m msg. LogAction m msg -> msg -> m Unit
unLogAction (LogAction f) = f

-- | A flipped `unLogAction`. Also available as the `&>` operator.
logActionFlipped :: forall m msg. msg -> LogAction m msg -> m Unit
logActionFlipped a action = unLogAction action a

-- | Run two actions one after another on the same message.
instance Apply m => Semigroup (LogAction m a) where
  append (LogAction a1) (LogAction a2) = LogAction \a -> a1 a *> a2 a

-- | The identity is the *null* logger: it ignores every message.
instance Applicative m => Monoid (LogAction m a) where
  mempty = LogAction \_ -> pure unit

instance Contravariant (LogAction m) where
  cmap f (LogAction action) = LogAction (action <<< f)

instance Apply m => Divide (LogAction m) where
  divide f (LogAction actionB) (LogAction actionC) =
    LogAction \a -> case f a of Tuple b c -> actionB b *> actionC c

instance Applicative m => Divisible (LogAction m) where
  conquer = mempty

instance Apply m => Decide (LogAction m) where
  choose f (LogAction actionB) (LogAction actionC) =
    LogAction (either actionB actionC <<< f)

instance Applicative m => Decidable (LogAction m) where
  lose f = LogAction (absurd <<< f)

-- | Apply a `LogAction` to a message. `action <& msg`.
infix 5 unLogAction as <&

-- | Flipped `<&`. `msg &> action`.
infix 5 logActionFlipped as &>

----------------------------------------------------------------------------
-- Semigroup combinators
----------------------------------------------------------------------------

-- | Combine a `Foldable` of actions into one (specialised `fold`).
foldActions :: forall f a m. Foldable f => Applicative m => f (LogAction m a) -> LogAction m a
foldActions actions = LogAction \a -> for_ actions \(LogAction action) -> action a

----------------------------------------------------------------------------
-- Contravariant combinators (monadic / partial variants not in the package)
----------------------------------------------------------------------------

-- | `cmap` whose conversion runs in the monad.
cmapM :: forall a b m. Bind m => (a -> m b) -> LogAction m b -> LogAction m a
cmapM f (LogAction action) = LogAction \a -> f a >>= action

-- | `cmap` for conversions that may drop the message.
cmapMaybe :: forall a b m. Applicative m => (a -> Maybe b) -> LogAction m b -> LogAction m a
cmapMaybe f (LogAction action) = LogAction \a -> maybe (pure unit) action (f a)

-- | Monadic `cmapMaybe`.
cmapMaybeM :: forall a b m. Monad m => (a -> m (Maybe b)) -> LogAction m b -> LogAction m a
cmapMaybeM f (LogAction action) = LogAction \a -> f a >>= maybe (pure unit) action

-- | Log only when the predicate holds for the message.
cfilter :: forall a m. Applicative m => (a -> Boolean) -> LogAction m a -> LogAction m a
cfilter predicate (LogAction action) = LogAction \a -> when (predicate a) (action a)

-- | Log only when the monadic predicate holds for the message.
cfilterM :: forall a m. Monad m => (a -> m Boolean) -> LogAction m a -> LogAction m a
cfilterM predicateM (LogAction action) = LogAction \a -> predicateM a >>= \b -> when b (action a)

-- | Replace every consumed message with a constant before logging.
cconst :: forall a b m. b -> LogAction m b -> LogAction m a
cconst b (LogAction action) = LogAction \_ -> action b

infixl 4 cconst as >$

----------------------------------------------------------------------------
-- Divisible / Decidable operators and monadic variants
----------------------------------------------------------------------------

-- | Operator for `divided` (`divide identity`): pairs two actions.
infixr 4 divided as >*<

-- | Operator for `chosen` (`choose identity`): routes via `Either`.
infixr 3 chosen as >|<

-- | Monadic `divide`.
divideM :: forall a b c m. Monad m => (a -> m (Tuple b c)) -> LogAction m b -> LogAction m c -> LogAction m a
divideM f (LogAction actionB) (LogAction actionC) =
  LogAction \a -> f a >>= \(Tuple b c) -> actionB b *> actionC c

-- | Monadic `choose`.
chooseM :: forall a b c m. Monad m => (a -> m (Either b c)) -> LogAction m b -> LogAction m c -> LogAction m a
chooseM f (LogAction actionB) (LogAction actionC) =
  LogAction \a -> f a >>= either actionB actionC

----------------------------------------------------------------------------
-- Comonad-style combinators (standalone — LogAction has no Functor/Comonad)
----------------------------------------------------------------------------

-- | Run a log action by feeding it `mempty`.
extract :: forall msg m. Monoid msg => LogAction m msg -> m Unit
extract (LogAction action) = action mempty

-- | Comonadic `extend`: chain transformations on messages.
extend :: forall msg m. Semigroup msg => (LogAction m msg -> m Unit) -> LogAction m msg -> LogAction m msg
extend f (LogAction action) = LogAction \m -> f (LogAction \m' -> action (m <> m'))

-- | `extend` with arguments flipped.
extendFlipped :: forall msg m. Semigroup msg => LogAction m msg -> (LogAction m msg -> m Unit) -> LogAction m msg
extendFlipped = flip extend

infixl 1 extendFlipped as =>>
infixr 1 extend as <<=

-- | Turn a single-message logger into one taking a pair, joined with `<>`.
duplicate :: forall msg m. Semigroup msg => LogAction m msg -> LogAction m (Tuple msg msg)
duplicate (LogAction l) = LogAction \(Tuple m1 m2) -> l (m1 <> m2)

-- | Like `duplicate` but for any `Foldable` of messages, joined with `fold`.
multiplicate :: forall f msg m. Foldable f => Monoid msg => LogAction m msg -> LogAction m (f msg)
multiplicate (LogAction l) = LogAction \msgs -> l (fold msgs)

-- | Like `multiplicate` but logs each message separately.
separate :: forall f msg m. Foldable f => Applicative m => LogAction m msg -> LogAction m (f msg)
separate (LogAction action) = LogAction (traverse_ action)

----------------------------------------------------------------------------
-- Higher-order
----------------------------------------------------------------------------

-- | Change the underlying monad of a `LogAction`.
hoistLogAction :: forall a m n. (m ~> n) -> LogAction m a -> LogAction n a
hoistLogAction f (LogAction l) = LogAction \a -> f (l a)
