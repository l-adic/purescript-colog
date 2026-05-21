-- | Ready-made `LogAction`s for `Message`, built from the core consumers.
-- |
-- | Colour is just a choice of formatter (the contravariant design at work):
-- | the console actions use the coloured formatter, the file actions use the
-- | plain one — so log files never contain ANSI escape codes. The `rich*`
-- | variants additionally prepend a UTC timestamp.
module Colog.Actions
  ( logMessageStdout
  , logMessageStderr
  , logMessageFile
  , richMessageStdout
  , richMessageStderr
  , richMessageFile
  ) where

import Colog.Core.Action (LogAction, cmapM, (>$<))
import Colog.Core.IO (logFileSync, logStringStderr, logStringStdout)
import Colog.Message (Message, fmtMessage, fmtMessageColored)
import Colog.Rich (defaultFields, fmtRichDefault, withFields)
import Effect.Class (class MonadEffect)

-- | Print messages to stdout (coloured).
logMessageStdout :: forall m. MonadEffect m => LogAction m Message
logMessageStdout = fmtMessageColored >$< logStringStdout

-- | Print messages to stderr (coloured).
logMessageStderr :: forall m. MonadEffect m => LogAction m Message
logMessageStderr = fmtMessageColored >$< logStringStderr

-- | Append messages to a file (plain — no colour codes).
logMessageFile :: forall m. MonadEffect m => String -> LogAction m Message
logMessageFile path = fmtMessage >$< logFileSync path

-- | Like `logMessageStdout` but prepends a UTC timestamp.
richMessageStdout :: forall m. MonadEffect m => LogAction m Message
richMessageStdout = withFields defaultFields (cmapM (fmtRichDefault fmtMessageColored) logStringStdout)

-- | Like `logMessageStderr` but prepends a UTC timestamp.
richMessageStderr :: forall m. MonadEffect m => LogAction m Message
richMessageStderr = withFields defaultFields (cmapM (fmtRichDefault fmtMessageColored) logStringStderr)

-- | Like `logMessageFile` but prepends a UTC timestamp (plain).
richMessageFile :: forall m. MonadEffect m => String -> LogAction m Message
richMessageFile path = withFields defaultFields (cmapM (fmtRichDefault fmtMessage) (logFileSync path))
