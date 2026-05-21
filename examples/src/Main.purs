-- | A tiny demo: a "backup script" that logs to the console AND a file at once.
-- |
-- | Run with: `spago run -p examples`
-- |
-- | The two consumers are combined with `<>` (the Monoid on `LogAction`), so
-- | every message is sent to both. Both use the timestamped/coloured
-- | `richMessage*` actions, so the file will also contain ANSI colour codes
-- | (a consequence of co-log's always-colour `showSeverity`).
module Main where

import Prelude

import Colog (LoggerT, Message, Msg(..), cmap, logDebug, logError, logInfo, logWarning, richMessageFile, richMessageStdout, usingLoggerT, withLog)
import Data.Foldable (for_)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

-- | Where the file consumer writes.
logPath :: String
logPath = "examples/output.log"

-- | Prepend a namespace tag to a message's text — used with `withLog`.
withNamespace :: String -> Message -> Message
withNamespace ns (Msg r) = Msg r { text = "[" <> ns <> "] " <> r.text }

-- | The dumb script. Note it never mentions a logger explicitly — `logInfo`
-- | etc. pull the `LogAction` from the `LoggerT` reader context, and `withLog`
-- | locally tags everything inside a block with a namespace.
app :: LoggerT Message Effect Unit
app = do
  logInfo "starting backup"
  for_ [ "photos", "docs", "music" ] \dir ->
    withLog (cmap (withNamespace dir)) do
      logDebug ("scanning " <> dir)
      logInfo ("backed up " <> dir)
  logWarning "disk almost full"
  logError "cloud upload failed, will retry"
  logInfo "backup finished"

main :: Effect Unit
main = do
  FS.writeTextFile UTF8 logPath "" -- start each run with a fresh log file
  let logger = richMessageStdout <> richMessageFile logPath
  usingLoggerT logger app
