-- | Re-exports the full @co-log@ API (plus all of @co-log-core@).
module Colog
  ( module Colog.Core
  , module Colog.Message
  , module Colog.Actions
  , module Colog.Monad
  , module Colog.Rich
  ) where

import Colog.Core (class Contravariant, class Decidable, class Decide, class Divide, class Divisible, class HasLog, Lens', LogAction(..), Severity(..), WithSeverity(..), cconst, cfilter, cfilterM, choose, chooseM, chosen, cmap, cmapM, cmapMaybe, cmapMaybeM, conquer, divide, divideM, divided, duplicate, extend, extendFlipped, extract, filterBySeverity, foldActions, getLogAction, getMsg, getSeverity, hoistLogAction, lens, logActionFlipped, logActionL, logFileSync, logNull, logPrint, logStringStderr, logStringStdout, lose, mapSeverity, multiplicate, overLogAction, separate, setLogAction, unLogAction, (&>), (<&), (<<=), (=>>), (>$), (>$<), (>*<), (>|<))
import Colog.Message (Message, Msg(..), SimpleMsg(..), fmtMessage, fmtMessageColored, fmtSimpleMessage, formatWith, log, logDebug, logError, logInfo, logWarning, showSeverity, showSeverityColored)
import Colog.Actions (logMessageFile, logMessageStderr, logMessageStdout, richMessageFile, richMessageStderr, richMessageStdout)
import Colog.Monad (class WithLog, LoggerT(..), liftLogAction, logMsg, logMsgs, runLoggerT, usingLoggerT, withLog)
import Colog.Rich (RichMsg(..), defaultFields, fmtRichDefault, nowUTC, showTime, withFields)
