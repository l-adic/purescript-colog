-- | Basic loggers working in `MonadEffect`.
-- |
-- | Port of @Colog.Core.IO@. These are `String`-based and simple — the three
-- | consumers requested for the skeleton live here:
-- |
-- | * `logNull`         — the null consumer (the `Monoid` identity)
-- | * `logStringStdout` — the console consumer
-- | * `logFileSync`     — the file consumer (synchronous append)
module Colog.Core.IO
  ( logNull
  , logStringStdout
  , logStringStderr
  , logPrint
  , logFileSync
  ) where

import Prelude

import Colog.Core.Action (LogAction(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

-- | The null logger: ignores every message. (`= mempty`.)
logNull :: forall m a. Applicative m => LogAction m a
logNull = mempty

-- | Print a `String` to stdout.
logStringStdout :: forall m. MonadEffect m => LogAction m String
logStringStdout = LogAction \msg -> liftEffect (Console.log msg)

-- | Print a `String` to stderr.
logStringStderr :: forall m. MonadEffect m => LogAction m String
logStringStderr = LogAction \msg -> liftEffect (Console.error msg)

-- | Print any `Show`able value to stdout.
logPrint :: forall a m. Show a => MonadEffect m => LogAction m a
logPrint = LogAction \a -> liftEffect (Console.logShow a)

-- | Append each message (followed by a newline) to a file, synchronously.
-- |
-- | A handle-based form (open once, append, close) is a natural future
-- | enhancement; this per-message append keeps the skeleton simple and pure
-- | `MonadEffect`.
logFileSync :: forall m. MonadEffect m => String -> LogAction m String
logFileSync path = LogAction \msg -> liftEffect (FS.appendTextFile UTF8 path (msg <> "\n"))
