-- | Message severity.
-- |
-- | Port of @Colog.Core.Severity@. The one-letter pattern synonyms (`D`/`I`/
-- | `W`/`E`) are dropped — PureScript has no pattern synonyms — so use the
-- | constructors directly. The derived `Ord` ordering is
-- | `Debug < Info < Warning < Error`, which is what `filterBySeverity` relies on.
module Colog.Core.Severity
  ( Severity(..)
  , filterBySeverity
  , WithSeverity(..)
  , getMsg
  , getSeverity
  , mapSeverity
  ) where

import Prelude

import Colog.Core.Action (LogAction, cfilter)

-- | Severity for log messages, from least to most severe.
data Severity = Debug | Info | Warning | Error

derive instance Eq Severity
derive instance Ord Severity

instance Show Severity where
  show Debug = "Debug"
  show Info = "Info"
  show Warning = "Warning"
  show Error = "Error"

instance Bounded Severity where
  bottom = Debug
  top = Error

-- | Keep only messages whose severity (via the projection) is at least the
-- | given threshold.
filterBySeverity :: forall m a. Applicative m => Severity -> (a -> Severity) -> LogAction m a -> LogAction m a
filterBySeverity sev fs = cfilter \a -> fs a >= sev

-- | A message tagged with a `Severity`, processable independently of it.
data WithSeverity msg = WithSeverity msg Severity

derive instance Functor WithSeverity

-- | The message payload.
getMsg :: forall msg. WithSeverity msg -> msg
getMsg (WithSeverity m _) = m

-- | The attached severity.
getSeverity :: forall msg. WithSeverity msg -> Severity
getSeverity (WithSeverity _ s) = s

-- | Map a function over the severity of a `WithSeverity`.
mapSeverity :: forall msg. (Severity -> Severity) -> WithSeverity msg -> WithSeverity msg
mapSeverity f (WithSeverity m s) = WithSeverity m (f s)
