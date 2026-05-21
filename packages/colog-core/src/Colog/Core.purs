-- | Re-exports all of @co-log-core@.
-- |
-- | The whole library is built on:
-- |
-- | ```purescript
-- | newtype LogAction m msg = LogAction (msg -> m Unit)
-- | ```
module Colog.Core
  ( module Colog.Core.Action
  , module Colog.Core.Class
  , module Colog.Core.IO
  , module Colog.Core.Severity
  ) where

import Colog.Core.Action (class Contravariant, class Decidable, class Decide, class Divide, class Divisible, LogAction(..), cconst, cfilter, cfilterM, choose, chooseM, chosen, cmap, cmapM, cmapMaybe, cmapMaybeM, conquer, divide, divideM, divided, duplicate, extend, extendFlipped, extract, foldActions, hoistLogAction, logActionFlipped, lose, multiplicate, separate, unLogAction, (&>), (<&), (<<=), (=>>), (>$), (>$<), (>*<), (>|<))
import Colog.Core.Class (class HasLog, Lens', getLogAction, lens, logActionL, overLogAction, setLogAction)
import Colog.Core.IO (logFileSync, logNull, logPrint, logStringStderr, logStringStdout)
import Colog.Core.Severity (Severity(..), WithSeverity(..), filterBySeverity, getMsg, getSeverity, mapSeverity)
