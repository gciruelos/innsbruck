module Main where

import Test.Hspec

import qualified PolynomialTest


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "Polynomial" PolynomialTest.spec
    describe "Groebner"   Groebner.spec

