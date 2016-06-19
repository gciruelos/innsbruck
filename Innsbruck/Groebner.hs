module Innsbruck.Groebner where

import Data.List
import Innsbruck.Polynomial
import Data.Tuple

sortedMultidegree :: Polynomial k -> [Integer] 
sortedMultidegree = monomial . sortedLeadingTerm

sortedLeadingMonomialLCM :: Polynomial k -> Polynomial k -> [Integer]
sortedLeadingMonomialLCM a b = 
    map (\x -> if uncurry (>) x then fst x else snd x) $
    zip (sortedMultidegree a) (sortedMultidegree b)

sortedSPolynomial :: (Eq k, Fractional k) => MonomialOrdering
                  -> Polynomial k -> Polynomial k -> Polynomial k
sortedSPolynomial order f g = 
    sortedDifference order (multTermPoly fMult f) (multTermPoly gMult g)
    where lcm = sortedLeadingMonomialLCM f g
          fMult = Term 1 (zipWith (-) lcm ((monomial.sortedLeadingTerm) f))
          gMult = Term 1 (zipWith (-) lcm ((monomial.sortedLeadingTerm) g))

sPolynomial :: (Eq k, Fractional k) => MonomialOrdering
            -> Polynomial k -> Polynomial k -> Polynomial k
sPolynomial order = sortAndApply order (sortedSPolynomial order)

criterionLCM :: (Eq k, Fractional k) => Polynomial k -> Polynomial k -> Bool
criterionLCM fi fj = 
    sortedLeadingMonomialLCM fi fj /=
    sortedMultidegree (
      multTermPoly (sortedLeadingTerm fi) (Polynomial [sortedLeadingTerm fj]))
    
computeSPolynomialReminder :: (Eq k, Fractional k) => MonomialOrdering 
                           -> [Polynomial k] -> (Polynomial k, Polynomial k)
                           -> Polynomial k
computeSPolynomialReminder order gs = snd .
                                    divPoly order gs .
                                    uncurry (sPolynomial order)

mapTuple f (x,y) = (f x, f y)

unrepeatedCartesianProduct :: Eq k => [Polynomial k]
                           -> [(Polynomial k, Polynomial k)]
unrepeatedCartesianProduct gs =
    nubBy (\x y -> mapTuple terms x == mapTuple terms y ||
                   mapTuple terms (swap x) == mapTuple terms y) 
    [(p, q) | p<-gs, q<-gs, terms p /= terms q]

groebner' :: (Eq k, Fractional k) => MonomialOrdering
          -> [Polynomial k] -> [Polynomial k]
groebner' order gs = filter (not.null.terms) $
                     map (computeSPolynomialReminder order gs)
                     (unrepeatedCartesianProduct gs)


groebnerStep :: (Eq k, Fractional k) => MonomialOrdering -> [Polynomial k]
             -> [Polynomial k]
groebnerStep order gs = gs ++ groebner' order gs

groebner :: (Eq k, Fractional k) => MonomialOrdering
         -> [Polynomial k] -> [Polynomial k] 
groebner order = until (\gs -> map terms (groebnerStep order gs) == map terms gs)
                 (groebnerStep order)

polyF :: Polynomial Float
polyF = Polynomial [Term 1 [3, 2], Term (-1) [2,3], Term 1 [1,0]]
polyG :: Polynomial Float
polyG = Polynomial [Term 3 [4, 1], Term 1 [0,2]]

test = sortedSPolynomial grlexOrder polyF polyG


poly1 = Polynomial [Term 1 [3,0], Term (-2) [1,1]]
poly2 = Polynomial [Term 1 [2,1], Term (-2) [0,2], Term 1 [1,0]]

test2 = sortedSPolynomial grlexOrder poly1 poly2

polysGroebner :: [Polynomial Float]
polysGroebner = [poly1, poly2] 

test3 = groebner grlexOrder [poly1, poly2]

