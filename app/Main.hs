module Main where

import MultiVariable
import TypeSafe
import Data.Vec.Lazy (Vec(..))

main :: IO ()
main = putStrLn $ pprint h

f = newPoly [(2, 2:::3:::4:::VNil), (3, 4 ::: 5:::6:::VNil)]

g = newPoly [(3, 1 ::: 0 ::: 2 ::: VNil), (5, 6:::2:::3:::VNil)]

h = mult f g
