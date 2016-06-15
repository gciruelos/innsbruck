module Innsbruck.Groebner where

import Data.List
import Innsbruck.Polynomial

sortedMultidegree :: Polynomial k -> [Integer] 
sortedMultidegree = monomial . sortedLeadingTerm

sortedLCM :: Polynomial k -> Polynomial k -> [Integer]
sortedLCM a b = map (\x -> if fst x > snd x then fst x else snd x) $
    zip (sortedMultidegree a) (sortedMultidegree b)

sortedSPolynomial order f g = 
    sortedDifference order (prodTermPoly fMult f) (prodTermPoly gMult g)
    where lcm = sortedLCM f g
          fMult = (Term 1 lcm) `divTerm` (sortedLeadingTerm f) 
          gMult = (Term 1 lcm) `divTerm` (sortedLeadingTerm g) 

sPolynomial order = sortAndApply order (sortedSPolynomial order)


polyF :: Polynomial Float
polyF = Polynomial [Term 1 [3, 2], Term (-1) [2,3], Term 1 [1,0]]
polyG :: Polynomial Float
polyG = Polynomial [Term 3 [4, 1], Term 1 [0,2]]
