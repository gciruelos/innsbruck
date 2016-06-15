module Innsbruck.Polynomial where

import Data.List

data Term k = Term 
    { coefficient :: k
    , monomial    :: [Integer] 
    }
    deriving (Eq)

data Polynomial k =  Polynomial {terms :: [Term k] }

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0..]

instance Show k => Show (Term k) where
  show (Term c m) = (show c) ++ " " ++ (intercalate " " monom)
    where monom :: [String]
          monom = ["x_" ++ (show i) ++ "^" ++ (show a) | 
                  (i, a) <- enumerate m, a > 0]

instance Show k => Show (Polynomial k) where
  show (Polynomial p) | null p    = "<empty>"
                      | otherwise = intercalate " + " (map show p)


type MonomialOrdering = [Integer] -> [Integer] -> Ordering


lexOrder :: MonomialOrdering
lexOrder [] []         = EQ
lexOrder [] (_:_)      = error "Different length lists in lexOrder."
lexOrder (_:_) []      = error "Different length lists in lexOrder."
lexOrder (a:as) (b:bs) | a > b     = GT
                       | a < b     = LT
                       | otherwise = lexOrder as bs

grlexOrder :: MonomialOrdering
grlexOrder as bs | as == bs = EQ
                 | otherwise = if a > b || (a == b && lexOrder as bs == GT)
                                  then GT
                                  else LT
                    where a = sum as
                          b = sum bs

grevlexAux :: Num a => [a] -> [a] -> [a]
grevlexAux as bs = revDiff as bs [] 
  where 
    revDiff [] [] l = l
    revDiff (x:xs) (y:ys) l = revDiff xs ys ((x - y):l)
    revDiff [] (_:_) _ = error "Different length lists in grevlexAux."
    revDiff (_:_) [] _ = error "Different length lists in grevlexAux."

grevlexOrder :: MonomialOrdering
grevlexOrder as bs | sum as > sum bs = GT
                   | sum as < sum bs = LT
                   | otherwise =
                     if null rightmostNonzero 
                     then EQ
                     else if head rightmostNonzero < 0 then GT else LT
  where rightmostNonzero = dropWhile (== 0) (grevlexAux as bs)

sortedLeadingTerm :: Polynomial k -> Term k
sortedLeadingTerm = head . terms

leadingTerm :: MonomialOrdering -> Polynomial k -> Term k
leadingTerm order (Polynomial f) = maximumBy (\p q -> order (monomial p) (monomial q)) f

multidegree :: MonomialOrdering -> Polynomial k -> [Integer] 
multidegree order = monomial . (leadingTerm order)

leadingCoefficient :: MonomialOrdering -> Polynomial k -> k
leadingCoefficient order = coefficient . (leadingTerm order)

leadingMonomial :: Num k => MonomialOrdering -> Polynomial k -> Polynomial k
leadingMonomial order f = Polynomial [Term 1 (multidegree order f)]

sortTerms :: MonomialOrdering -> Polynomial k -> Polynomial k
sortTerms order = Polynomial . (sortBy (\a b -> order (monomial b) (monomial a))) . terms

negateTerm :: Num k => Term k -> Term k
negateTerm t = Term (negate (coefficient t)) (monomial t)

sortAndApply :: MonomialOrdering -> (Polynomial k -> Polynomial k -> a)
            -> Polynomial k -> Polynomial k -> a
sortAndApply order f p q = f (sortTerms order p) (sortTerms order q)


{- Sorted sum functions. -}
sortedSumTerm :: (Eq k, Num k) => MonomialOrdering -> Term k -> [Term k] -> [Term k]
sortedSumTerm _     f []     = [f]
sortedSumTerm order f (g:gs) = case order (monomial f) (monomial g) of
    EQ -> let c = (coefficient f) + (coefficient g)
          in if c == 0 then gs else (Term c (monomial g)) : gs
    GT -> f : g : gs
    LT -> g : (sortedSumTerm order f gs)

sortedSumTerms :: (Eq k, Num k) => MonomialOrdering -> [Term k] -> [Term k] -> [Term k]
sortedSumTerms _     [] g = g
sortedSumTerms _      f [] = f
sortedSumTerms order (f:fs) gs = sortedSumTerms order fs (sortedSumTerm order f gs)

sortedSum :: (Eq k,  Num k) => MonomialOrdering -> Polynomial k -> Polynomial k
          -> Polynomial k
sortedSum order f g = Polynomial $ sortedSumTerms order (terms f) (terms g)

sortedDifference :: (Eq k,  Num k) => MonomialOrdering -> Polynomial k -> Polynomial k
                 -> Polynomial k
sortedDifference order f g = Polynomial $
    sortedSumTerms order (terms f) (map negateTerm (terms g))
{- END: Sorted sum functions. -}

dividesTerm :: Term k -> Term k -> Bool
dividesTerm f g = all id (zipWith (<=) (monomial f) (monomial g))

-- Can fail.
-- Pre: f | g.
divTerm :: Fractional k => Term k -> Term k -> Term k
divTerm f g = Term ((coefficient f) / coefficient g) (zipWith (-) (monomial f) (monomial g))

prodTermPoly :: Num k => Term k -> Polynomial k -> Polynomial k
prodTermPoly t f = Polynomial [Term (c * (coefficient term)) (addExponents (monomial term)) | term <- terms f]
                 where c = coefficient t
                       addExponents = zipWith (+) (monomial t)

{- Division Algorithm in k[x1, ..., xn].
 -
 - Input: f, (f1, ..., fs).
 - Output: (q1, ..., qs), r.
 -
 - While p != 0 Do
 -     i := 1
 -     divisionocurred := false
 -     While i <= s And divisionoccurred = false Do
 -         If LT(fi) divides LT(p) Then
 -             qi := qi + LT(p)/LT(fi)
 -             p := p - LT(p)/LT(fi) fi
 -             divisionoccurred := true
 -         Else
 -             i := i + 1
 -     If divisionoccurred = false Then
 -         r := r + LT(p)
 -         p := p - LT(p)
 - Return (q1, ..., qs)
 -}

divWhileDivisible :: (Eq k, Fractional k) => MonomialOrdering -> Polynomial k
                  -> (Polynomial k, Polynomial k)
                  -> (Polynomial k, Polynomial k)
divWhileDivisible order fi (p, qi) = 
    if (sortedLeadingTerm fi) `dividesTerm` (sortedLeadingTerm p)
    then divWhileDivisible order fi
        (sortedSum order p (prodTermPoly (negateTerm lTR) fi), sortedSum order qi lTRatio)
    else (p, qi)
      where lTR = divTerm (sortedLeadingTerm p) (sortedLeadingTerm fi)
            lTRatio = Polynomial [lTR] 

-- Inner loop.
divStep :: (Eq k, Fractional k) => MonomialOrdering -> Polynomial k
        -> [Polynomial k] -> [Polynomial k]
        -> ([Polynomial k], Polynomial k) 
divStep _     p []      _       = ([], p)
divStep _     p (_:_)   []      = ([], p)
divStep order p (fi:fs) (qi:qs) = 
    if (sortedLeadingTerm fi) `dividesTerm` (sortedLeadingTerm p)
    then ((sortedSum order qi qiRatio):qs, (sortedSum order p pRatio))
    else (qi:(fst recursiveCall), (snd recursiveCall))
      where lTR = divTerm (sortedLeadingTerm p) (sortedLeadingTerm fi)
            qiRatio = Polynomial [lTR]
            pRatio = (prodTermPoly (negateTerm lTR) fi)
            recursiveCall = divStep order p fs qs
                           
isZero :: Polynomial k -> Bool
isZero = null . terms

zeroPolynomial :: Polynomial k
zeroPolynomial = Polynomial []

divPoly' :: (Eq k, Fractional k) => MonomialOrdering -> Polynomial k
         -> [Polynomial k] -> [Polynomial k] -> Polynomial k
         -> ([Polynomial k], Polynomial k)
divPoly' order p fs qs r = 
    if isZero p
    then (qs, r)
    else if terms p == terms pNext
         then divPoly' order (sortedDifference order p pLT) fs qs (sortedSum order r pLT)
         else divPoly' order  pNext fs qiNext r
    where (qiNext, pNext) = divStep order p fs qs
          pLT = Polynomial [sortedLeadingTerm p]

divPoly :: (Eq k, Fractional k) => MonomialOrdering -> Polynomial k -> [Polynomial k]
        -> ([Polynomial k], Polynomial k)
divPoly order p fs = divPoly' order p_ fs_ (take s (repeat zeroPolynomial)) zeroPolynomial
                   where fs_ = map (sortTerms order) fs
                         p_ = sortTerms order p
                         s = length fs

