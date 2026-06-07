module Multivariable where

import Data.List (partition)
import Data.Maybe (catMaybes)
import Numeric.Natural (Natural)

-- desired powers, length is number of variables
type Monomial = [Natural]

totalDegree :: Monomial -> Natural
totalDegree = sum

-- substitutes in values for the variables
evalM :: Monomial -> [Integer] -> Maybe Integer
evalM m vals
  | length m == length vals = Just $ product [x | (a, b) <- zip vals m, let x = a ^ b]
  | otherwise = Nothing

-- first arg is coefficients, second is list of powers for that term
type MultiPoly = [(Integer, Monomial)]

newMultiPoly :: [Integer] -> [Monomial] -> Maybe MultiPoly
newMultiPoly coeffs ms
  | length coeffs /= length ms = Nothing
  | otherwise = Just $ zip coeffs ms

evalMP :: MultiPoly -> [Integer] -> Maybe Integer
evalMP ms vals = sum <$> sequenceA [fmap (k *) (evalM m vals) | (k, m) <- ms]

getPs :: MultiPoly -> [Monomial]
getPs ms = snd <$> ms

getCs :: MultiPoly -> [Integer]
getCs ms = fst <$> ms

combineM :: MultiPoly -> MultiPoly
combineM [] = []
combineM (m@(c, powers) : ms) =
  let (same, rest) = partition (\x -> snd x == powers) ms
      coeffSum = (sum . getCs) (m : same)
   in if coeffSum == 0
        then combineM rest
        else (coeffSum, powers) : combineM rest

addM :: MultiPoly -> MultiPoly -> MultiPoly
addM m1 m2 = combineM $ m1 ++ m2

multM :: Monomial -> Monomial -> Maybe Monomial
multM m1 m2
  | length m1 /= length m2 = Nothing
  | otherwise = Just $ zipWith (+) m1 m2

distributeM :: Monomial -> MultiPoly -> Maybe MultiPoly
distributeM _ [] = Just []
distributeM m1 ((coeff, m) : ms)
  | length m1 /= length m = Nothing
  | otherwise = (:) <$> sequenceA (coeff, multM m1 m) <*> distributeM m1 ms

multMP :: MultiPoly -> MultiPoly -> Maybe MultiPoly
multMP m1 m2 = combineM <$> sequenceA [sequenceA (c1 * c2, multM p1 p2) | (c1, p1) <- m1, (c2, p2) <- m2]

-- multP :: Polynomial -> Polynomial -> Polynomial
-- multP t1s t2s = combine [mult t1 t2 | t1 <- t1s, t2 <- t2s]
m1 :: MultiPoly
m1 = [(1, [2, 3]), (2, [2, 4])]

m2 :: MultiPoly
m2 = [(2, [4, 5]), (3, [1, 3]), (5, [3, 1])]
