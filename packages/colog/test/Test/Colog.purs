module Test.Colog where

import Prelude

import Colog (LogAction(..), Message, Msg(..), RichMsg(..), Severity(..), cmap, fmtMessage, fmtRichDefault, logDebug, logInfo, showSeverityColored, showTime, usingLoggerT, withLog, withSpan)
import Data.Array (head, length, snoc)
import Data.DateTime (DateTime)
import Data.Either (isLeft)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | Prefix a message's text — used to test `withLog`'s local transformation.
addPrefix :: Message -> Message
addPrefix (Msg r) = Msg r { text = "app:" <> r.text }

-- | A fixed, deterministic time for timestamp tests (the minimum DateTime).
fixedTime :: DateTime
fixedTime = bottom

msgText :: Message -> String
msgText (Msg r) = r.text

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: Spec Unit
spec = describe "Colog" do

  describe "Message formatting" do
    it "fmtMessage is plain (no ANSI), severity label + text" do
      fmtMessage (Msg { severity: Info, text: "hi" }) `shouldEqual` "[Info]    hi"
      contains (Pattern "\x1b[") (fmtMessage (Msg { severity: Info, text: "hi" })) `shouldEqual` false

    it "showSeverityColored adds an ANSI colour escape" do
      contains (Pattern "[Info]") (showSeverityColored Info) `shouldEqual` true
      contains (Pattern "\x1b[") (showSeverityColored Info) `shouldEqual` true

  describe "LoggerT / WithLog / withLog (HasLog self-instance)" do
    it "logs through the reader context, with local transformation" do
      ref <- liftEffect $ Ref.new []
      let capture = LogAction \(m :: Message) -> Ref.modify_ (\arr -> snoc arr (fmtMessage m)) ref
      liftEffect $ usingLoggerT capture do
        logInfo "x"
        withLog (cmap addPrefix) (logDebug "y")
      out <- liftEffect $ Ref.read ref
      let joined = joinWith "\n" out
      length out `shouldEqual` 2
      contains (Pattern "x") joined `shouldEqual` true
      contains (Pattern "app:y") joined `shouldEqual` true -- proves withLog's cmap ran

  describe "RichMsg / timestamping" do
    it "fmtRichDefault prepends the timestamp" do
      let
        rm = RichMsg
          { msg: Msg { severity: Info, text: "hi" }
          , fields: { utcTime: (pure fixedTime :: Effect DateTime) }
          }
      result <- liftEffect $ fmtRichDefault fmtMessage rm
      result `shouldEqual` (showTime fixedTime <> fmtMessage (Msg { severity: Info, text: "hi" }))

    it "allows extra fields via the open row" do
      let
        rm = RichMsg
          { msg: Msg { severity: Debug, text: "z" }
          , fields: { utcTime: (pure fixedTime :: Effect DateTime), reqId: "r1" }
          }
      result <- liftEffect $ fmtRichDefault fmtMessage rm
      result `shouldEqual` (showTime fixedTime <> fmtMessage (Msg { severity: Debug, text: "z" }))

  -- withSpan is monad-generic; here we exercise it in LoggerT Message Aff (one of
  -- many valid stacks) via usingLoggerT, capturing the Messages it logs.
  describe "withSpan (logs via context, exception-safe)" do
    it "logs a span message carrying the label" do
      ref <- liftEffect $ Ref.new []
      let cap = LogAction \(m :: Message) -> liftEffect (Ref.modify_ (\a -> snoc a m) ref)
      usingLoggerT cap (withSpan "ok" (pure unit))
      out <- liftEffect $ Ref.read ref
      length out `shouldEqual` 1
      (contains (Pattern "ok took") <<< msgText <$> head out) `shouldEqual` Just true

    it "logs the span even when the body throws, then rethrows" do
      ref <- liftEffect $ Ref.new []
      let cap = LogAction \(m :: Message) -> liftEffect (Ref.modify_ (\a -> snoc a m) ref)
      result <- attempt (usingLoggerT cap (withSpan "boom" (liftEffect (throwException (error "x")))) :: Aff Unit)
      out <- liftEffect $ Ref.read ref
      isLeft result `shouldEqual` true -- the exception propagated
      length out `shouldEqual` 1 -- but the span was still logged
      (contains (Pattern "boom") <<< msgText <$> head out) `shouldEqual` Just true
