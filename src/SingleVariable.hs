module SingleVariable where

import Data.Bifunctor (first)
import Data.List (partition, sortBy)
import Data.Ord (comparing)
import Numeric.Natural (Natural)

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- TODO add multivariable polynomials

-- TODO make this polymorphic over possible fields
type Coefficient = Integer

type Power = Natural

data Term = Term Coefficient Power
  deriving (Eq, Show)

-- TODO make this polymorphic over the field where the coefficients come from
type Polynomial = [Term]

pprint :: Polynomial -> String
pprint ts = dropPlus $ go ts
  where
    dropPlus = dropWhile (\c -> c == ' ' || c == '+')
    go [] = []
    go (t : ts) = pprint' t ++ go ts
      where
        pprint' :: Term -> String
        pprint' (Term 0 _) = []
        pprint' (Term 1 1) = " + " ++ "x"
        pprint' (Term 1 pow)
          | pow == 0 = " + " ++ show 1
          | otherwise = " + " ++ "x^" ++ show pow
        pprint' (Term coef 1)
          | coef == 0 = []
          | coef < 0 = " - " ++ show (abs coef) ++ "x"
          | otherwise = " + " ++ show coef ++ "x"
        pprint' (Term coef 0)
          | coef < 0 = " - " ++ show (abs coef)
          | coef == 0 = []
          | otherwise = " + " ++ show coef
        pprint' (Term coef pow)
          | coef < 0 = " - " ++ show (abs coef) ++ "x^" ++ show pow
          | coef == 0 = []
          | otherwise = " + " ++ show coef ++ "x^" ++ show pow

newPoly :: [Integer] -> Polynomial
newPoly ps = reverse $ go (reverse ps) 0
  where
    go :: [Integer] -> Natural -> [Term]
    go [] _ = []
    go (t : ts) n
      | t == 0 = go ts (n + 1)
      | otherwise = Term t n : go ts (n + 1)

sortTerms :: Polynomial -> Polynomial
sortTerms = sortBy (flip (comparing getP))

getC :: Term -> Coefficient
getC (Term c _) = c

getP :: Term -> Power
getP (Term _ p) = p

degree :: Polynomial -> Natural
degree [] = 0
degree ts = let ts' = combine ts in maximum (fmap getP ts')

getCoefficients :: Polynomial -> [Integer]
getCoefficients [] = []
getCoefficients (t : ts) = go ps maxpower
  where
    ps = sortTerms $ combine (t : ts)
    maxpower = maximum $ getP <$> ps
    go :: [Term] -> Natural -> [Integer]
    go [] _ = []
    go (x : xs) n
      | getP x == n = getC x : go xs (n - 1)
      | otherwise = 0 : go (x : xs) (n - 1)

eval :: (Num a) => Term -> a -> a
eval (Term c p) x = fromInteger c * (x ^ p)

evalP :: (Num a) => Polynomial -> a -> a
evalP [] _ = 0
evalP (t : ts) x = eval t x + evalP ts x

mult :: Term -> Term -> Term
mult (Term c1 p1) (Term c2 p2) = Term (c1 * c2) (p1 + p2)

combine :: Polynomial -> Polynomial
combine [] = []
combine (t : ts) =
  let (same, rest) = partition (\x -> getP x == getP t) ts
      coeffSum = sum (map getC (t : same))
   in if coeffSum == 0
        then combine rest
        else Term coeffSum (getP t) : combine rest

distribute :: Term -> Polynomial -> Polynomial
distribute t1 ts = combine $ fmap (mult t1) ts

addP :: Polynomial -> Polynomial -> Polynomial
addP t1s t2s = combine $ t1s ++ t2s

multP :: Polynomial -> Polynomial -> Polynomial
multP t1s t2s = combine [mult t1 t2 | t1 <- t1s, t2 <- t2s]

rationalRoots :: Polynomial -> [(Integer, Integer)]
rationalRoots [] = []
rationalRoots ts =
  [ (p, q)
    | p <- factor $ abs $ getC (last ts),
      q <- factor $ abs $ getC (head ts)
  ]
  where
    factor n = [k | k <- [1 .. n], n `mod` k == 0]

rationalRootsEval :: Polynomial -> [(Integer, Integer)]
rationalRootsEval poly =
  filter (\x -> abs (evalP poly (toDecimal x)) < 0.000001) $ roots ++ fmap (first negate) roots
  where
    roots = rationalRoots poly
    toDecimal :: (Integer, Integer) -> Double
    toDecimal (n, d) = fromInteger n / fromInteger d

diff :: Term -> Term
diff (Term _ 0) = Term 0 0
diff (Term coef pow) = Term (coef * toInteger pow) (pow - 1)

diffP :: Polynomial -> Polynomial
diffP = fmap diff

d :: Int -> Polynomial -> Polynomial
d n f = iterate diffP f !! n

diffPN :: Polynomial -> [Polynomial]
diffPN poly = take ((fromInteger . toInteger) (degree poly) + 1) $ iterate diffP poly

-- TODO can't do this yet since the coefficients are restricted to integers
integrate :: Term -> Term
integrate (Term c p) = undefined

newtonRahpson :: Polynomial -> Double -> Integer -> Double
newtonRahpson f x0 iter = go x0 1
  where
    df = diffP f
    newton x = x - (evalP f x / evalP df x)
    go x n
      | n > iter = x
      | otherwise = go (newton x) (n + 1)

bisection :: Polynomial -> Double -> Double -> Integer -> Maybe Double
bisection p min max iter = go p min max 1
  where
    go p min max n
      | n > iter = Just m
      | f min * f max > 0 = Nothing
      | f min * f m < 0 = go p min m (n + 1)
      | otherwise = go p m max (n + 1)
      where
        f = evalP p
        m = (min + max) / 2
