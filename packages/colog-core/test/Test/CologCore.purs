module Test.CologCore where

import Prelude

import Colog.Core (LogAction(..), Severity(..), choose, cmap, cfilter, divide, filterBySeverity, foldActions, logFileSync, logNull, separate, (<&))
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | A `LogAction` that records every `String` it receives, plus a reader.
mkCapture :: Effect { logger :: LogAction Effect String, read :: Effect (Array String) }
mkCapture = do
  ref <- Ref.new []
  pure
    { logger: LogAction \s -> Ref.modify_ (\arr -> snoc arr s) ref
    , read: Ref.read ref
    }

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: Spec Unit
spec = describe "Colog.Core" do

  describe "Monoid (the null consumer)" do
    it "logNull is a left identity" do
      cap <- liftEffect mkCapture
      liftEffect $ (logNull <> cap.logger) <& "a"
      out <- liftEffect cap.read
      out `shouldEqual` [ "a" ]

    it "logNull is a right identity" do
      cap <- liftEffect mkCapture
      liftEffect $ (cap.logger <> logNull) <& "a"
      out <- liftEffect cap.read
      out `shouldEqual` [ "a" ]

    it "<> runs both actions in order" do
      cap <- liftEffect mkCapture
      liftEffect $ (cap.logger <> cmap (_ <> "!") cap.logger) <& "x"
      out <- liftEffect cap.read
      out `shouldEqual` [ "x", "x!" ]

    it "logNull alone produces no output" do
      cap <- liftEffect mkCapture
      liftEffect $ logNull <& "ignored"
      out <- liftEffect cap.read
      out `shouldEqual` []

  describe "Contravariant message transformation" do
    it "cmap transforms the message before logging" do
      cap <- liftEffect mkCapture
      liftEffect $ cmap (_ <> "!") cap.logger <& "x"
      out <- liftEffect cap.read
      out `shouldEqual` [ "x!" ]

    it "cfilter drops messages failing the predicate" do
      cap <- liftEffect mkCapture
      let logger = cfilter (_ /= "skip") cap.logger
      liftEffect $ logger <& "skip"
      liftEffect $ logger <& "keep"
      out <- liftEffect cap.read
      out `shouldEqual` [ "keep" ]

    it "filterBySeverity keeps only >= threshold" do
      cap <- liftEffect mkCapture
      let logger = filterBySeverity Warning identity (cmap show cap.logger)
      liftEffect $ logger <& Debug
      liftEffect $ logger <& Error
      out <- liftEffect cap.read
      out `shouldEqual` [ "Error" ]

  describe "Divisible / Decidable routing" do
    it "choose routes via Either" do
      cap <- liftEffect mkCapture
      let logger = choose identity cap.logger (cmap show cap.logger)
      liftEffect $ logger <& Left "L"
      liftEffect $ logger <& (Right 5 :: Either String Int)
      out <- liftEffect cap.read
      out `shouldEqual` [ "L", "5" ]

    it "divide splits one message across two actions" do
      cap <- liftEffect mkCapture
      let logger = divide (\s -> Tuple s (s <> s)) cap.logger cap.logger
      liftEffect $ logger <& "a"
      out <- liftEffect cap.read
      out `shouldEqual` [ "a", "aa" ]

  describe "Foldable / comonad-style combinators" do
    it "foldActions runs every action" do
      cap <- liftEffect mkCapture
      liftEffect $ foldActions [ cap.logger, cmap (_ <> "!") cap.logger ] <& "x"
      out <- liftEffect cap.read
      out `shouldEqual` [ "x", "x!" ]

    it "separate logs each element of a Foldable" do
      cap <- liftEffect mkCapture
      liftEffect $ separate cap.logger <& [ "a", "b", "c" ]
      out <- liftEffect cap.read
      out `shouldEqual` [ "a", "b", "c" ]

  describe "The file consumer" do
    it "logFileSync appends newline-terminated lines" do
      let path = "colog-core-test.log"
      liftEffect $ FS.writeTextFile UTF8 path ""
      liftEffect $ logFileSync path <& "line1"
      liftEffect $ logFileSync path <& "line2"
      contents <- liftEffect $ FS.readTextFile UTF8 path
      contents `shouldEqual` "line1\nline2\n"
      liftEffect $ FS.unlink path
