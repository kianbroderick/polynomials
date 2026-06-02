module Main where

import Data.List (partition, sortBy)
import Data.Ord (comparing)

main :: IO ()
main = putStrLn "Hello, Haskell!"

newtype Polynomial = Polynomial [Integer]

instance Show Polynomial where
  show (Polynomial xs) = drop 3 $ go xs (length xs - 1)
    where
      go [] _ = ""
      go (c : cs) n
        | n == 0 = " + " ++ show c ++ go cs (n - 1)
        | n == 1 = " + " ++ show c ++ "x"
        | c == 0 = go cs (n - 1)
        | c == 1 = " + " ++ "x" ++ "^" ++ show n ++ go cs (n - 1)
        | c > 0 = " + " ++ show c ++ "x" ++ "^" ++ show n ++ go cs (n - 1)
        | c < 0 = " - " ++ show (abs c) ++ "x" ++ "^" ++ show n ++ go cs (n - 1)

eval :: Polynomial -> Double -> Double
eval (Polynomial []) _ = 0
eval (Polynomial (c : cs)) x = fromIntegral c * x + eval (Polynomial cs) x

addP :: Polynomial -> Polynomial -> Polynomial
addP (Polynomial xs) (Polynomial ys) = Polynomial $ reverse $ takeWhile (/= 0) $ zipWith (+) xs' ys'
  where
    xs' = reverse xs ++ repeat 0
    ys' = reverse ys ++ repeat 0

type Coefficient = Integer

type Power = Integer

data Term = Term Coefficient Power

newPoly :: [Integer] -> [Term]
newPoly xs = toTerm (Polynomial xs)

instance Show Term where
  show (Term 0 _) = show 0
  show (Term c 0) = show c
  show (Term c 1) = show c ++ "x"
  show (Term c p) = show c ++ "x" ++ "^" ++ show p

toTerm :: Polynomial -> [Term]
toTerm (Polynomial []) = []
toTerm ps = reverse $ go ps 0
  where
    go :: Polynomial -> Integer -> [Term]
    go (Polynomial []) _ = []
    go (Polynomial (p : ps)) n
      | p == 0 = go (Polynomial ps) (n + 1)
      | otherwise = Term p n : go (Polynomial ps) (n + 1)

sortTerms :: [Term] -> [Term]
sortTerms = sortBy (flip (comparing getP))
  where
    getP (Term _ p) = p

toPoly :: [Term] -> Polynomial
toPoly [] = Polynomial []
toPoly (t : ts) = let (p : ps) = sortTerms $ combineT (t : ts) in Polynomial $ go ps (getP p)
  where
    getP (Term _ p) = p
    getC (Term c _) = c
    go :: [Term] -> Integer -> [Integer]
    go [] _ = []
    go (x : xs) n
      | getP x == n = [getC x] ++ go xs (n - 1)
      | otherwise = go xs (n - 1)

evalT :: Term -> Integer -> Integer
evalT (Term c p) x = c * (x ^ p)

multT :: Term -> Term -> Term
multT (Term c1 p1) (Term c2 p2) = Term (c1 * c2) (p1 + p2)

combineT :: [Term] -> [Term]
combineT [] = []
combineT (t : ts) =
  let (same, rest) = partition (\x -> getP x == getP t) ts
      coeffSum = sum (map getC (t : same))
   in if coeffSum == 0
        then combineT rest
        else Term coeffSum (getP t) : combineT rest
  where
    getP (Term _ p) = p
    getC (Term c _) = c

distribute :: Term -> [Term] -> [Term]
distribute t1 ts = combineT $ fmap (multT t1) ts

multTP :: [Term] -> [Term] -> [Term]
multTP t1s t2s = combineT [multT t1 t2 | t1 <- t1s, t2 <- t2s]

p1 = newPoly [1, 2, 3]

p2 = newPoly [4, 5, 6]
