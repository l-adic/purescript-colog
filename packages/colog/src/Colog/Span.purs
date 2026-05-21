-- | Timing spans: run an action, log how long it took.
-- |
-- | Backed by `Effect.Aff.bracket`, PureScript's native exception- and
-- | cancellation-safe bracket: the duration is recorded in the release step,
-- | which always runs — even if the body throws or the fibre is killed — and
-- | the original outcome (value or exception) is preserved afterwards.
-- |
-- | The span emits a structured `SpanInfo` carrying the duration as a typed
-- | `Milliseconds` field; how that is rendered is the consumer's choice (via
-- | `cmap`/`fmtSpan`), not baked into the combinator.
module Colog.Span
  ( SpanInfo
  , withSpan
  , fmtSpan
  ) where

import Prelude

import Colog.Core.Action (LogAction, (<&))
import Colog.Core.Severity (Severity(..))
import Colog.Message (Message, Msg(..))
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, bracket)
import Effect.Class (liftEffect)
import Effect.Now (now)

-- | What a span reports: a label and the measured duration.
type SpanInfo = { label :: String, duration :: Milliseconds }

-- | Run an `Aff` action and log a `SpanInfo` with the measured duration. The
-- | duration is logged whether the action succeeds, throws, or is cancelled;
-- | on failure the exception still propagates afterwards.
withSpan :: forall a. LogAction Aff SpanInfo -> String -> Aff a -> Aff a
withSpan logger label action =
  bracket
    (liftEffect now)
    ( \start -> do
        end <- liftEffect now
        let duration = Milliseconds (unwrap (unInstant end) - unwrap (unInstant start))
        logger <& { label, duration }
    )
    (\_ -> action)

-- | A default rendering of a `SpanInfo` as a `Message` (at `Info`). Compose it
-- | with any `Message` consumer: `fmtSpan >$< richMessageStdout`.
fmtSpan :: SpanInfo -> Message
fmtSpan { label, duration } =
  Msg { severity: Info, text: label <> " took " <> show (unwrap duration) <> "ms" }
