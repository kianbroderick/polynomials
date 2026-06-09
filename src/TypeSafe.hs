{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeSafe where

import Data.Nat
import qualified Data.Vec.Lazy as V
import Data.Vec.Lazy (Vec(..))
import Data.List (partition, intercalate, sortBy)
import Data.Type.Nat
import Data.Functor.Day
import Data.Proxy
import Data.Ord (comparing)
import Data.Function 

-- fixed length of n vector of natural numbers, n is number of variables, values are powers
data Monomial k n = Monomial (V.Vec n Nat) deriving (Eq, Show)

-- k is field where coefficients come from, n is number of variables
data Term k n where
  Term :: (Num k, Eq k, Show k) => k -> Monomial k n -> Term k n

instance (Num k, Eq k) => Eq (Term k n) where
  (Term k1 m1) == (Term k2 m2) = k1 == k2 && m1 == m2

instance (Show k) => Show (Term k n) where
  show (Term k1 m1) = "Term " ++ show k1 ++ " " ++ show m1

-- polynomial in n variables with coefficients in a field k
data Polynomial k n = Polynomial [Term k n] deriving (Eq, Show)

class (SNatI (NumVars a)) => Variables a k | a -> k where
  type NumVars a :: Nat
  pprint :: a -> String
  degree :: a -> Nat
  numVar :: a -> Nat
  numVar _ = reflect @(NumVars a) Proxy
  eval :: (Num k) => a -> Vec (NumVars a) k -> k

instance SNatI n => Variables (Monomial k n) k where
  type NumVars (Monomial k n) = n
  pprint (Monomial (x:::VNil)) = "x^" ++ show x
  pprint (Monomial (x:::y:::VNil)) = "(x^" ++ show x ++ ")(y^" ++ show y ++ ")"
  pprint (Monomial (x:::y:::z:::VNil)) = "(x^" ++ show x ++ ")(y^" ++ show y ++ ")(z^" ++ show z ++ ")"
  pprint (Monomial powers) = go (V.toList powers) 1
    where
      go :: [Nat] -> Integer -> String
      go [] _ = []
      go (p:ps) n = "(x" ++ show n ++ ")" ++ "^" ++ show p ++ go ps (n+1)
  degree (Monomial powers) = foldr (+) 0 powers
  numVar (Monomial powers) = fromIntegral $ length powers
  eval (Monomial powers) vals = foldr (*) 1 $ V.zipWith (\b p -> b ^ p) vals powers

instance SNatI n => Variables (Term k n) k where
  type NumVars (Term k n) = n
  pprint (Term 0 _) = ""
  pprint (Term 1 m) = pprint m
  pprint (Term coeff m) = show coeff ++ " * " ++ pprint m
  degree (Term _ m) = degree m
  eval (Term coeff (Monomial powers)) vals = coeff * (foldr (*) 1 $ V.zipWith (\b p -> b ^ p) vals powers)

instance SNatI n => Variables (Polynomial k n) k where
  type NumVars (Polynomial k n) = n
  pprint (Polynomial ps) = intercalate " + " $ map pprint ps
  degree (Polynomial ps) = maximum $ fmap degree ps
  eval (Polynomial ts) vals = foldr (+) 0 $ map ((flip eval) vals) ts

class MMult a k | a -> k where
  mult :: a -> a -> a

instance MMult (Monomial k n) k where
  mult (Monomial p1) (Monomial p2) = Monomial $ V.zipWith (+) p1 p2

instance MMult (Term k n) k where
  mult (Term c1 m1) (Term c2 m2) = Term (c1 * c2) (mult m1 m2)

instance MMult (Polynomial k n) k where
  mult (Polynomial t1s) (Polynomial t2s) = Polynomial $ dap $ day (fmap mult t1s) t2s

class MAdd a k | a -> k where
  add :: a -> a -> a

class Poly a k | a -> k where
  scale :: a -> k -> a
  combine :: a -> a
  normalOrder :: a -> a

instance Poly (Term k n) k where
  scale (Term coeff n) scalar = Term (scalar * coeff) n
  combine = id
  normalOrder = id

instance MAdd (Polynomial k n) k where
  add (Polynomial t1s) (Polynomial t2s) = combine $ Polynomial $ t1s ++ t2s

  --overalpping instances

addTerm :: Term k n -> Term k n -> Maybe (Term k n)
addTerm (Term c1 m1) (Term c2 m2)
  | m1 == m2 = Just $ Term (c1 + c2) m1
  | otherwise = Nothing

instance Poly (Polynomial k n) k where
  scale (Polynomial ts) scalar = Polynomial $ fmap ((flip scale) scalar) ts
  combine (Polynomial []) = Polynomial []
  combine (Polynomial (t@(Term _ m):ts)) = 
    let (same, rest) = partition (\(Term _ m') -> m == m') ts
        coeffSum = sum $ map (\(Term c _) -> c) (t:same)
      in if coeffSum == 0
        then combine (Polynomial rest)
        else let (Polynomial r) = combine (Polynomial rest) in Polynomial ((Term coeffSum m) : r)
  -- GHC really does not like this definition here
  -- normalOrder (Polynomial ts) = Polynomial $ map (\ t@(Term p q) -> if degree q == degree q then t else t) ts
  
newTerm ::(Num k, Eq k, Show k) => k -> Vec n Nat -> Term k n
newTerm k p = Term k (Monomial p)

newPoly :: (Num k, Eq k, Show k) => [(k, Vec n Nat)] -> Polynomial k n
newPoly [] = Polynomial []
newPoly ts = Polynomial $ fmap (\(c, m) -> newTerm c m) ts

m1 = Monomial (1 ::: 2 ::: 3 ::: VNil)

m2 = Monomial (4 ::: 5 ::: 6 ::: VNil)

t1 = Term 5 m1

t2 = Term 2.0 m2

p1 = newPoly [(1, 3 ::: 4 ::: 5 ::: VNil), 
  (2, 2 ::: 3 ::: 500 ::: VNil), 
  (2, 3 ::: 4 ::: 5 ::: VNil), 
  (0, 1 ::: 2 ::: 3 ::: VNil)]

p2 = Polynomial [t1, t2]

p3 = add p1 p2

p4 = mult p1 p3
