module PolynomialTest where

import Innsbruck.Polynomial
import Test.Hspec

equalTerms a b = (terms a) == (terms b)

instance Eq k => Eq (Polynomial k) where
  p == q = (terms p) == (terms q)

spec :: Spec
spec =  do
  describe "Innsbruck.Polynomial" $ do
    it "lexOrder" $ do
      (lexOrder [1,2,0] [0,3,4]) `shouldBe` GT
      (lexOrder [3,2,1] [3,2,4]) `shouldBe` LT
      (lexOrder [] []) `shouldBe` EQ
    it "grlexOrder" $ do
      (grlexOrder [1,2,3] [3,2,0]) `shouldBe` GT
      (grlexOrder [1,1,5] [1,2,4]) `shouldBe` LT
      (grlexOrder [] []) `shouldBe` EQ
    it "grevlexOrder" $ do
      (grevlexOrder [4,7,1] [4,2,3]) `shouldBe` GT
      (grevlexOrder [4,1,3] [1,5,2]) `shouldBe` LT
      (grevlexOrder [] []) `shouldBe` EQ
    it "monomialOrders" $ do
      terms (sortTerms lexOrder examplePoly) `shouldBe`
          [Term (-5) [3,0,0], Term 7 [2,0,2], Term 4 [1,2,1], Term 4 [0,0,2]]
      terms (sortTerms grlexOrder examplePoly) `shouldBe`
          [Term 7 [2,0,2], Term 4 [1,2,1], Term (-5) [3,0,0], Term 4 [0,0,2]]
      terms (sortTerms grevlexOrder examplePoly) `shouldBe`
          [Term 4 [1,2,1], Term 7 [2,0,2], Term (-5) [3,0,0], Term 4 [0,0,2]]
    it "generalDefinitions" $ do
      (multidegree lexOrder examplePoly) `shouldBe` [3,0,0]
      (leadingCoefficient lexOrder examplePoly) `shouldBe` (-5)
      (leadingMonomial lexOrder examplePoly) `shouldSatisfy` 
          (equalTerms (Polynomial [Term 1 [3,0,0]]))
      (leadingTerm lexOrder examplePoly) `shouldBe` (Term (-5) [3,0,0])
      (divPoly lexOrder exampleP exampleFs) `shouldBe` 
                  ([Polynomial [Term 1 [1,0], Term 1 [0,1]],
                    Polynomial [Term 1 [0,0]]],
                                  Polynomial [Term 1 [1,0],
                                              Term 1 [0,1],
                                              Term 1 [0,0]])

examplePoly :: Polynomial Int
examplePoly = Polynomial 
    [ Term (-5) [3,0,0]
    , Term 7 [2,0,2]
    , Term 4 [1,2,1]
    , Term 4 [0,0,2]
    ]



exampleP :: Polynomial Float
exampleP = Polynomial
    [ Term 1 [2, 1]
    , Term 1 [1, 2]
    , Term 1 [0, 2]
    ]

exampleFs :: [Polynomial Float]
exampleFs = [Polynomial [ Term 1 [1, 1]
                        , Term (-1) [0, 0]
                        ],
             Polynomial [ Term 1 [0, 2]
                        , Term (-1) [0, 0]
                        ]]



