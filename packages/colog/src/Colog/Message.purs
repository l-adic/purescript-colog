-- | Severity-tagged log messages and their formatters.
-- |
-- | Port of @Colog.Message@. The `CallStack`/source-location field is dropped
-- | (no PureScript analogue), so a `Msg` is just a severity plus text.
-- |
-- | `fmtMessage`/`showSeverity` are **plain** (no ANSI), so they are safe for
-- | files and pipes. The `*Colored` variants add ANSI colour. The ready-made
-- | console actions in "Colog.Actions" pick colour automatically based on
-- | whether the destination is a TTY (see "Colog.Tty").
module Colog.Message
  ( Msg(..)
  , Message
  , SimpleMsg(..)
  , log
  , logDebug
  , logInfo
  , logWarning
  , logError
  , fmtMessage
  , fmtMessageColored
  , fmtSimpleMessage
  , showSeverity
  , showSeverityColored
  , formatWith
  ) where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Colog.Core.Action (LogAction, cmap)
import Colog.Core.Severity (Severity(..))
import Colog.Monad (class WithLog, logMsg)

-- | A message carrying a severity (or any tag) and text.
newtype Msg sev = Msg { severity :: sev, text :: String }

-- | A `Msg` tagged with `Severity` — the common case.
type Message = Msg Severity

-- | A message with no severity, only text.
newtype SimpleMsg = SimpleMsg { text :: String }

-- | Log text at the given severity.
log :: forall sev env m. WithLog env (Msg sev) m => sev -> String -> m Unit
log severity text = logMsg (Msg { severity, text })

-- | Log at `Debug` severity.
logDebug :: forall env m. WithLog env Message m => String -> m Unit
logDebug = log Debug

-- | Log at `Info` severity.
logInfo :: forall env m. WithLog env Message m => String -> m Unit
logInfo = log Info

-- | Log at `Warning` severity.
logWarning :: forall env m. WithLog env Message m => String -> m Unit
logWarning = log Warning

-- | Log at `Error` severity.
logError :: forall env m. WithLog env Message m => String -> m Unit
logError = log Error

-- | Format a `Message` as `"[Severity] text"` (plain, no colour).
fmtMessage :: Message -> String
fmtMessage (Msg { severity, text }) = showSeverity severity <> text

-- | Like `fmtMessage` but with an ANSI-coloured severity.
fmtMessageColored :: Message -> String
fmtMessageColored (Msg { severity, text }) = showSeverityColored severity <> text

-- | Format a `SimpleMsg` (just its text).
fmtSimpleMessage :: SimpleMsg -> String
fmtSimpleMessage (SimpleMsg { text }) = text

-- | Render a severity as a bracketed, padded label (plain).
showSeverity :: Severity -> String
showSeverity = case _ of
  Debug -> "[Debug]   "
  Info -> "[Info]    "
  Warning -> "[Warning] "
  Error -> "[Error]   "

-- | Like `showSeverity` but ANSI-coloured (Debug=green, Info=blue,
-- | Warning=yellow, Error=red).
showSeverityColored :: Severity -> String
showSeverityColored = case _ of
  Debug -> colored Green "[Debug]   "
  Info -> colored Blue "[Info]    "
  Warning -> colored Yellow "[Warning] "
  Error -> colored Red "[Error]   "
  where
  colored :: Color -> String -> String
  colored c = withGraphics (foreground c)

-- | Turn a `String` logger into a `msg` logger via a formatting function
-- | (an alias for `cmap`).
formatWith :: forall m msg. (msg -> String) -> LogAction m String -> LogAction m msg
formatWith = cmap
