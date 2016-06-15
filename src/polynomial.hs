module Polynomial where

import Data.List

data Term k = Term 
    { coefficient :: k
    , monomial    :: [Integer] 
    }
    deriving (Eq)

enumerate = zip [0..]

instance Show k => Show (Term k) where
  show (Term c m) = (show c) ++ " " ++ (intercalate " " monomial)
    where monomial :: [String]
          monomial = ["x_" ++ (show i) ++ "^" ++ (show a) | 
                     (i, a) <- enumerate m, a > 0]

data Polynomial k =  Polynomial [Term k]

instance Show k => Show (Polynomial k) where
  show (Polynomial p) = intercalate " + " (map show p)



lexOrder [] []         = EQ
lexOrder [] (_:bs)     = EQ  -- Error.
lexOrder (_:as) []     = EQ  -- Error.
lexOrder (a:as) (b:bs) | a > b     = GT
                       | a < b     = LT
                       | otherwise = lexOrder as bs

grlexOrder as bs | as == bs = EQ
                 | otherwise = if a > b || (a == b && lexOrder as bs == GT)
                                  then GT
                                  else LT
                    where a = sum as
                          b = sum bs

grevlexAux as bs = revDiff as bs [] 
  where 
    revDiff [] [] l = l
    revDiff (a:as) (b:bs) l = revDiff as bs ((a - b):l)

grevlexOrder as bs = if null rightmostNonzero 
                     then EQ
                     else if head rightmostNonzero < 0 then GT else LT
  where rightmostNonzero = dropWhile (== 0) (grevlexAux as bs)


leadingTerm :: ([Integer] -> [Integer] -> Ordering) -> Polynomial k -> Polynomial k
leadingTerm order (Polynomial f) = Polynomial [maximumBy 
    (\p q -> order (monomial p) (monomial q)) f]

-- multidegree = monomial . leadingTerm

-- leadingMonomial = monomial . leadingTerm

-- leadingCoefficient = coefficient . leadingTerm

