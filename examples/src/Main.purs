-- | One demo that exercises the whole library: a "backup script" that
-- |
-- |   * logs to the console (coloured) AND a file (plain) at once,
-- |   * timestamps every line,
-- |   * namespaces sub-tasks with `withLog`,
-- |   * times each step with `withSpan` (bracket-backed), and
-- |   * recovers from a failing step whose span still logs its duration.
-- |
-- | Run with: `spago run -p examples`
module Main where

import Prelude

import Colog (LogAction, Message, Msg(..), SpanInfo, cmap, fmtSpan, logDebug, logError, logInfo, logWarning, richMessageFile, richMessageStdout, usingLoggerT, withLog, withSpan, (>$<))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error, message, throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

-- | Where the file consumer writes.
logPath :: String
logPath = "examples/output.log"

-- | Prepend a namespace tag to a message's text — used with `withLog`.
withNamespace :: String -> Message -> Message
withNamespace ns (Msg r) = Msg r { text = "[" <> ns <> "] " <> r.text }

main :: Effect Unit
main = launchAff_ do
  liftEffect (FS.writeTextFile UTF8 logPath "") -- fresh log file each run
  let
    -- one logger fanning out to BOTH console (coloured) and file (plain), each timestamped
    msgLogger = (richMessageStdout <> richMessageFile logPath) :: LogAction Aff Message
    -- spans render through the same logger
    spanLogger = fmtSpan >$< msgLogger
    -- run a reader-style logging block (logInfo/withLog/…) in Aff
    runLog = usingLoggerT msgLogger

  -- time the whole backup; each step is timed too
  withSpan spanLogger "backup" do
    runLog (logInfo "starting backup")

    for_ [ "photos", "docs", "music" ] \dir ->
      withSpan spanLogger ("backup " <> dir) do
        delay (Milliseconds 20.0)
        runLog $ withLog (cmap (withNamespace dir)) do
          logDebug ("scanning " <> dir)
          logInfo ("backed up " <> dir)

    runLog (logWarning "disk almost full")

    -- a step that throws: its span still logs the duration, and we recover
    result <- attempt $ withSpan spanLogger "cloud upload" do
      delay (Milliseconds 15.0)
      liftEffect (throwException (error "network down"))
    case result of
      Left e -> runLog (logError ("cloud upload failed: " <> message e))
      Right _ -> pure unit
