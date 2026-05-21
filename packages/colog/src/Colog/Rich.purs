-- | Extensible "rich" messages with effectful fields (timestamp, etc.).
-- |
-- | The native PureScript replacement for co-log's `RichMsg`/`FieldMap`. Where
-- | Haskell needs an open type family + a `DMap TypeRep` dependent map, a
-- | PureScript **record** already is a heterogeneous, extensible, statically
-- | typed map from labels to differently-typed values — and the row variable
-- | gives the same open-world extensibility as the open type family.
-- |
-- | Each field stores an *effectful producer* (`m a`) that is run at output
-- | time (so a timestamp reflects when the message is logged, not when
-- | `logInfo` was called). Extend the field set with ordinary record literals
-- | or `Record.insert` (whose `Row.Lacks` constraint rejects duplicate labels);
-- | a formatter requires exactly the fields it names via `Row.Cons`.
module Colog.Rich
  ( nowUTC
  , showTime
  , RichMsg(..)
  , defaultFields
  , withFields
  , fmtRichDefault
  ) where

import Prelude

import Colog.Core.Action (LogAction, cmap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (now)

-- | The current time as a UTC `DateTime`.
nowUTC :: Effect DateTime
nowUTC = toDateTime <$> now

-- | Format a time as `"[YYYY-MM-DD HH:mm:ss.SSS] "` (UTC, ISO-8601-ish).
showTime :: DateTime -> String
showTime dt =
  "[" <> either (const "?") identity (formatDateTime "YYYY-MM-DD HH:mm:ss.SSS" dt) <> "] "

-- | A base message bundled with a record of effectful field producers. The
-- | `fields` row is the native analogue of co-log's `FieldMap`. The `m`
-- | parameter is phantom here (it appears inside the field types by convention,
-- | e.g. `utcTime :: m DateTime`).
newtype RichMsg (m :: Type -> Type) (msg :: Type) (fields :: Row Type) = RichMsg
  { msg :: msg
  , fields :: Record fields
  }

-- | The default field set: a single `utcTime` field that reads the clock.
defaultFields :: forall m. MonadEffect m => Record (utcTime :: m DateTime)
defaultFields = { utcTime: liftEffect nowUTC }

-- | Attach a field record to a base action (co-log's `upgradeMessageAction`).
withFields
  :: forall m msg fields
   . Record fields
  -> LogAction m (RichMsg m msg fields)
  -> LogAction m msg
withFields fields = cmap \msg -> RichMsg { msg, fields }

-- | Format a rich message by running its `utcTime` field and prepending the
-- | formatted time to the formatted base message. The open row
-- | `(utcTime :: m DateTime | r)` requires the field set to contain `utcTime`
-- | while allowing any additional fields — plain record access, no typeclass
-- | machinery.
-- |
-- | Pass `fmtMessage` for plain output or `fmtMessageColored` for colour.
fmtRichDefault
  :: forall m msg r
   . Monad m
  => (msg -> String)
  -> RichMsg m msg (utcTime :: m DateTime | r)
  -> m String
fmtRichDefault fmt (RichMsg rec) = do
  t <- rec.fields.utcTime
  pure (showTime t <> fmt rec.msg)
