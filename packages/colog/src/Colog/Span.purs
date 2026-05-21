-- | Timing spans: run an action, log how long it took.
-- |
-- | Monad-agnostic: it reads the logger from context (`WithLog`/`HasLog`), the
-- | clock via `MonadEffect`, and gets exception-safety from `MonadError` — so it
-- | works in any stack that provides those (transformer stacks, `purescript-run`
-- | with an except effect, your own app monad, …), not just `LoggerT`.
-- |
-- | The duration is logged even when the body throws, after which the error
-- | propagates. The measured `SpanInfo` keeps the duration as a typed
-- | `Milliseconds` field; `withSpanBy` chooses how to render it.
module Colog.Span
  ( SpanInfo
  , withSpan
  , withSpanBy
  , fmtSpan
  ) where

import Prelude

import Colog.Core.Severity (Severity(..))
import Colog.Message (Message, Msg(..))
import Colog.Monad (class WithLog, logMsg)
import Control.Monad.Error.Class (class MonadError, throwError, try)
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (now)

-- | What a span reports: a label and the measured duration.
type SpanInfo = { label :: String, duration :: Milliseconds }

-- | Time an action and log the span (as a `Message`, via `fmtSpan`).
withSpan
  :: forall env m e a
   . WithLog env Message m
  => MonadEffect m
  => MonadError e m
  => String
  -> m a
  -> m a
withSpan = withSpanBy fmtSpan

-- | `withSpan` with a custom rendering of the `SpanInfo` into the context's
-- | message type — keep the duration structured (JSON, metrics, …) if you like.
withSpanBy
  :: forall env msg m e a
   . WithLog env msg m
  => MonadEffect m
  => MonadError e m
  => (SpanInfo -> msg)
  -> String
  -> m a
  -> m a
withSpanBy toMsg label action = do
  start <- liftEffect now
  result <- try action
  end <- liftEffect now
  let duration = Milliseconds (unwrap (unInstant end) - unwrap (unInstant start))
  logMsg (toMsg { label, duration })
  either throwError pure result

-- | Default rendering of a `SpanInfo` as a `Message` (at `Info`).
fmtSpan :: SpanInfo -> Message
fmtSpan { label, duration } =
  Msg { severity: Info, text: label <> " took " <> show (unwrap duration) <> "ms" }
