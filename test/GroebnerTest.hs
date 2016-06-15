module GroebnerTest where

import Innsbruck.Groebner
import Test.Hspec

spec :: Spec
spec =  do
  describe "Innsbruck.Polynomial" $ do
    it "lexOrder" $ do
