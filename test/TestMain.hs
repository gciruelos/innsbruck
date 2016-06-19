module Main where

import Test.Hspec

import qualified PolynomialTest
import qualified GroebnerTest


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "Polynomial" PolynomialTest.spec
    describe "Groebner"   GroebnerTest.spec

