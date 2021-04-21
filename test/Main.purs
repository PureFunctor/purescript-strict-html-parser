module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Sync (readFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.HTML.Parser (parseTags)


spec :: Spec Unit
spec = do
  describe "parseTags" do
    it "successfully parses a real-world document" do
      html <- liftEffect $
        Buffer.toString Encoding.UTF8 =<< readFile "test/index.html"
      parseTags html `shouldNotEqual` Left { error: "arbitrary error", pos: -1 }


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec
