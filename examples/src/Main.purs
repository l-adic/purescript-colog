-- | One demo exercising the whole library: a "backup script" that
-- |
-- |   * logs to the console (coloured) AND a file (plain) at once,
-- |   * timestamps every line,
-- |   * namespaces sub-tasks with `withLog`,
-- |   * times each step with `withSpan`, and
-- |   * recovers from a failing step whose span still logs its duration.
-- |
-- | The logging env is a separate pure binding; `script` never threads a logger
-- | (everything reads it from context); `main` just wires them together.
-- |
-- | Run with: `spago run -p examples`
module Main where

import Prelude

import Colog (LogAction, LoggerT, Message, Msg(..), cmap, logDebug, logError, logInfo, logWarning, richMessageFile, richMessageStdout, usingLoggerT, withLog, withSpan)
import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, message, throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

logPath :: String
logPath = "examples/output.log"

-- | Prepend a namespace tag to a message's text — used with `withLog`.
withNamespace :: String -> Message -> Message
withNamespace ns (Msg r) = Msg r { text = "[" <> ns <> "] " <> r.text }

-- | The logging environment (pure): one action fanning out to console + file.
env :: { logger :: LogAction Aff Message }
env = { logger: richMessageStdout <> richMessageFile logPath }

-- | The fake script. No logger is threaded — `logInfo`/`withSpan`/`withLog` all
-- | read it from the `LoggerT` context.
script :: LoggerT Message Aff Unit
script = withSpan "backup" do
  logInfo "starting backup"

  for_ [ "photos", "docs", "music" ] \dir ->
    withSpan ("backup " <> dir) $ withLog (cmap (withNamespace dir)) do
      liftAff (delay (Milliseconds 20.0))
      logDebug ("scanning " <> dir)
      logInfo ("backed up " <> dir)

  logWarning "disk almost full"

  -- a step that throws: its span still logs the duration, and we recover
  result <- try $ withSpan "cloud upload" do
    liftAff (delay (Milliseconds 15.0))
    void (liftEffect (throwException (error "network down")))
  case result of
    Left e -> logError ("cloud upload failed: " <> message e)
    Right _ -> pure unit

main :: Effect Unit
main = launchAff_ do
  liftEffect (FS.writeTextFile UTF8 logPath "") -- fresh log file each run
  usingLoggerT env.logger script
