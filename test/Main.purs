module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (callByName)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter (consoleReporter)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ]
    $ do
      testCallByName

testCallByName :: Spec Unit
testCallByName =
  describe "callByName" do
    it "foo" do
      "foo" `shouldEqual` "foo"