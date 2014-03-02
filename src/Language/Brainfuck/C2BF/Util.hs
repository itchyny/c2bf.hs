module Language.Brainfuck.C2BF.Util where
import Control.Monad (join)
import Control.Monad.Instances ()
import Data.List (minimumBy)
import Data.Functor ((<$>))

-- | dropLastWhile f == reverse . dropWhile f . reverse, but revese costs.
dropLastWhile :: (a -> Bool) -> [a] -> [a]
dropLastWhile _ [] = []
dropLastWhile f bf
  = let (hea, res) = span f bf
        in if null res
              then []
              else hea ++ let (he, re) = span (not . f) res
                              in he ++ dropLastWhile f re

-- | x -> a * b + c for the elements of the list, common a
multtogether :: Integral a => [a] -> [(a, a, a)]
multtogether xs = minimumBy sorter $ (\a -> divs a <$> xs) <$> [1..15]
  where
    divs a x = let b = div (x + div a 2) a in (a, b, x - a * b)
    sorterf y = (\(a, _, _) -> a) (head y) 
              + sum (map (\(_, b, c) -> abs b + abs c) y)
    sorter ys zs = if sorterf ys > sorterf zs then GT else LT

-- | x -> a * b + c
partconst :: Integral t => t -> Either t (t, t, t)
partconst x | abs x < 16 = Left x
            | x < 0      = let Right a = partconst (-x) in Right (neg a)
            | otherwise  = Right x'
  where
    divx = div x
    neg (a, b, c) = (a, -b, -c)
    sorter (a, b, c) (d, e, f) = if a + b + abs c > d + e + abs f
                                    then GT else LT
    x' = minimumBy sorter $
           join (map (\a -> [(a, divx a, x - a * divx a),
                             (a, 1 + divx a, x - a - a * divx a)])
                              [3 .. round(sqrt(fromIntegral x) :: Double)])

-- | exists only one element which satisfies is equal to the argument
existOne :: Eq a => a -> [a] -> Bool
existOne x = (==1) . length . filter (==x)

