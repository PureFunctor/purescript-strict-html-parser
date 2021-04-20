module Test.Main where

import Prelude

import Data.List (List(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Sync (readFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.HTML.Parser (parseTags)


spec :: Spec Unit
spec = do
  describe "parseTags" do
    it "successfully parses a real-world document" do
      html <- liftEffect $
        Buffer.toString Encoding.UTF8 =<< readFile "test/index.html"
      parseTags html `shouldNotEqual` Nil


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec
