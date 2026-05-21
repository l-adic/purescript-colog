-- | The reader-based context-logging layer.
-- |
-- | Port of @Colog.Monad@. `LoggerT` is a `ReaderT` carrying a `LogAction` in
-- | its context (the env type is recursive, exactly as in Haskell), and
-- | `WithLog` is the constraint synonym tying together "monad has the env" and
-- | "env has a `LogAction`". `HasCallStack` is dropped (no PureScript analogue).
module Colog.Monad
  ( LoggerT(..)
  , runLoggerT
  , class WithLog
  , logMsg
  , logMsgs
  , withLog
  , liftLogAction
  , usingLoggerT
  ) where

import Prelude

import Colog.Core.Action (LogAction(..), hoistLogAction)
import Colog.Core.Class (class HasLog, getLogAction, overLogAction)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, asks, local)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Foldable (class Foldable, traverse_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

-- | A `ReaderT` that keeps a `LogAction` in its context. The context's monad is
-- | `LoggerT` itself, so logging actions can themselves log.
newtype LoggerT msg m a = LoggerT (ReaderT (LogAction (LoggerT msg m) msg) m a)

-- | Unwrap a `LoggerT` to its underlying `ReaderT`.
runLoggerT :: forall msg m a. LoggerT msg m a -> ReaderT (LogAction (LoggerT msg m) msg) m a
runLoggerT (LoggerT r) = r

derive newtype instance Functor m => Functor (LoggerT msg m)
derive newtype instance Apply m => Apply (LoggerT msg m)
derive newtype instance Applicative m => Applicative (LoggerT msg m)
derive newtype instance Bind m => Bind (LoggerT msg m)
derive newtype instance Monad m => Monad (LoggerT msg m)
derive newtype instance MonadEffect m => MonadEffect (LoggerT msg m)
derive newtype instance MonadAff m => MonadAff (LoggerT msg m)
derive newtype instance MonadThrow e m => MonadThrow e (LoggerT msg m)
derive newtype instance MonadError e m => MonadError e (LoggerT msg m)
derive newtype instance Monad m => MonadAsk (LogAction (LoggerT msg m) msg) (LoggerT msg m)
derive newtype instance Monad m => MonadReader (LogAction (LoggerT msg m) msg) (LoggerT msg m)

instance MonadTrans (LoggerT msg) where
  lift = LoggerT <<< lift

-- | Constraint: monad `m` can read an environment `env`, and `env` carries a
-- | `LogAction m msg`. (PureScript has no tuple constraint synonyms, so this is
-- | a class synonym: a class whose superclasses are the bundled constraints,
-- | with a single universal instance. The `HasLog` fundep `env -> msg m`
-- | propagates through the superclass, so message/monad inference still works.)
class (MonadAsk env m, HasLog env msg m) <= WithLog env msg m

instance (MonadAsk env m, HasLog env msg m) => WithLog env msg m

-- | Log a single message, pulling the `LogAction` from the environment.
logMsg :: forall env msg m. WithLog env msg m => msg -> m Unit
logMsg msg = do
  LogAction action <- asks getLogAction
  action msg

-- | Log every message in a `Foldable`.
logMsgs :: forall f env msg m. Foldable f => WithLog env msg m => f msg -> m Unit
logMsgs = traverse_ logMsg

-- | Run a block with the logging action locally transformed.
withLog
  :: forall env msg m a
   . MonadReader env m
  => HasLog env msg m
  => (LogAction m msg -> LogAction m msg)
  -> m a
  -> m a
withLog = local <<< overLogAction

-- | Lift a `LogAction` so it can log in a transformed monad.
liftLogAction :: forall t m msg. Monad m => MonadTrans t => LogAction m msg -> LogAction (t m) msg
liftLogAction = hoistLogAction lift

-- | Run a `LoggerT` with the given base `LogAction`.
usingLoggerT :: forall msg m a. Monad m => LogAction m msg -> LoggerT msg m a -> m a
usingLoggerT action logger = runReaderT (runLoggerT logger) (liftLogAction action)
