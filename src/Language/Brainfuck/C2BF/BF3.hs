module Language.Brainfuck.C2BF.BF3
  ( BF3Code
  , BF3 (..)
  , convert32
  , convert32Option
  , convert23
  )
  where
import Control.Monad (join)
import Control.Monad.Instances
import Data.Functor ((<$>))
import Data.List (sortBy, nub, delete)
import qualified Data.Map.Lazy as Map
import Language.Brainfuck.C2BF.BF0
import Language.Brainfuck.C2BF.BF1
import Language.Brainfuck.C2BF.BF2
import Language.Brainfuck.C2BF.Util

-- | Layer 3 Brainfuck
--
-- This layer gathers multiple Incrs and Decrs. Moreover, synonyms for `If
-- statement` and `variable Free`, which are compiled to while loops, are
-- defined.
type BF3Code = [BF3]
data BF3 = AddI3 BFPointer BFInt
         | SubI3 BFPointer BFInt
         | Mult3 BFPointer [(BFInt, BFPointer)]
         | While3 BFPointer BF3Code
         | If3 BFPointer BF3Code
         | Free3 BFPointer
         | Put3 BFPointer
         | Get3 BFPointer
         | Raw3 BFCode BFPointer BFPointer
         deriving (Eq, Show)

instance BF BF3 where
  convertOption opt = convertOption opt . convert32Option opt
  upconvert = convert23 . upconvert

instance BFRaw BF3 where
  raw = Raw3
convert32Option :: [Option] -> BF3Code -> BF2Code
convert32Option opt
  | elem LazyInitialization opt
    = convert32Option (delete LazyInitialization opt) . lazyInit
  | elem TailingFree opt
    = convert32Option (delete TailingFree opt) . dropLastFrees
  | elem HeadingWhile opt
    = convert32Option (delete HeadingWhile opt) . dropHeadWhile
  | elem InitializeNear opt
    = convert32Option (delete InitializeNear opt) . initializeNear
  | elem DropInnerFree opt
    = convert32Option (delete DropInnerFree opt) . dropInnerFrees
  | otherwise
    = convert32'

convert32 :: BF3Code -> BF2Code
convert32 = convert32Option defaultOption

convert32' :: BF3Code -> BF2Code
convert32' (AddI3 q num : bf)
  | num < 0 = convert32' (SubI3 q (-num) : bf)
  | otherwise = replicate num (Incr2 q) ++ convert32' bf
convert32' (SubI3 q num : bf)
  | num < 0 = convert32' (AddI3 q (-num) : bf)
  | otherwise = replicate num (Decr2 q) ++ convert32' bf
convert32' (Mult3 _ [] : bf) = convert32' bf
convert32' (m@(Mult3 p [(n, q)]) : bf)
  | p == q = if n == 1 then convert32' bf
                       else error $ "error: same index " ++ show m
convert32' (Mult3 p q : bf)
  = While2 p (Decr2 p : join (map replIncrDecr (sortBy sorter (filter nonP q))))
  : convert32' bf
    where replIncrDecr (num, dest) | num < 0 = replicate (-num) (Decr2 dest)
                                   | otherwise = replicate num (Incr2 dest)
          nonP (_, r) = p /= r
          sorter (_, a) (_, b) = if a > b then GT else LT
convert32' (While3 p bf' : bf) = While2 p (convert32' bf')
                               : convert32' (dropSameFree bf)
  where dropSameFree (b@(Free3 q) : bfs) | p == q = bfs
                                         | otherwise = b : dropSameFree bfs
        dropSameFree bfs = bfs
convert32' (If3 p bf' : bf) = convert32' (While3 p (bf' ++ [Free3 p]) : bf)
convert32' bf@(Free3 _ : _)
  -- Optimization. Collect all frees and sort them.
  = let (frees, rest) = span isFree3 bf
        whileDecr (Free3 x) = While2 x [ Decr2 x ]
        unFree3 (Free3 x) = x
        sorter (Free3 a) (Free3 b)
          | unFree3 (head frees) >= unFree3 (last frees)
                      = if a < b then GT else LT
          | otherwise = if a > b then GT else LT
        in if null frees
              then convert32' rest
              else map whileDecr (sortBy sorter (nub frees)) ++ convert32' rest
-- convert32' (Free3 q : bf) = While2 q [Decr2 q] : convert32' bf
convert32' (Put3 q : bf) = Put2 q : convert32' bf
convert32' (Get3 q : bf) = Get2 q : convert32' bf
convert32' (Raw3 b p q : bf) = Raw2 b p q : convert32' bf
convert32' [] = []

-- | Easy optimization for last Free3
dropLastFrees :: BF3Code -> BF3Code
dropLastFrees = dropLastWhile isFree3
-- dropLastFrees = reverse . dropWhile isFree3 . reverse -- not efficient

-- | Deep optimization for inner Free3
dropInnerFrees :: BF3Code -> BF3Code
dropInnerFrees = fst . dropInnerFrees' Map.empty

type BF3State = Map.Map BFPointer BF3Flag
data BF3Flag = Zero | DontKnow deriving (Eq, Ord, Show)

dropInnerFrees' :: BF3State -> BF3Code -> (BF3Code, BF3State)
dropInnerFrees' m (a@(AddI3 p _) : bf)
  = (a:) <$$> dropInnerFrees' (insDontKnows [p] m) bf
dropInnerFrees' m (a@(SubI3 p _) : bf)
  = (a:) <$$> dropInnerFrees' (insDontKnows [p] m) bf
dropInnerFrees' m (a@(Mult3 p q) : bf)
  = (a:) <$$> dropInnerFrees' (insDontKnows (map snd q) $ insZero p m) bf
dropInnerFrees' m (While3 p bfs : bf)
  -- This code is dangerous as well as that of If3.
  -- Because, if p is Zero at running time, variable frees inside bfs
  -- are not effective so this dropInnerFrees mistakes.
  = let (bfs', m') = dropInnerFrees' m bfs
        in (While3 p bfs':) <$$> dropInnerFrees' (insZero p m') bf
dropInnerFrees' m (If3 p bfs : bf)
  = let (bfs', m') = dropInnerFrees' m bfs
        in (If3 p bfs':) <$$> dropInnerFrees' (insZero p m') bf
dropInnerFrees' m (a@(Free3 p) : bf)
  = (case Map.lookup p m of
          Just Zero -> id
          _ -> (a:)) <$$> insZero p <$> dropInnerFrees' m bf
dropInnerFrees' m (a@(Put3 _) : bf)
  = (a:) <$$> dropInnerFrees' m bf
dropInnerFrees' m (a@(Get3 p) : bf)
  = (a:) <$$> dropInnerFrees' (insDontKnows [p] m) bf
dropInnerFrees' m (a@(Raw3 _ p q) : bf)
  | rawModulo p == a && p == q
    = (a:) <$$> insZero p
           <$> insDontKnows [p+1,p+2,p+3,p+4]
           <$> dropInnerFrees' m bf
  | rawModulo' p == a && p == q
    = (a:) <$$> insZero p
           <$> insDontKnows [p+1,p+2,p+3]
           <$> dropInnerFrees' m bf
  -- Following code cannot analyze what happens in the raw code, so it can
  -- result in a broken BF code. In short, dropInnerFrees' is dangerous,
  -- when Raw3 is used.
  | otherwise = (a:) <$$> dropInnerFrees' m bf
dropInnerFrees' m bf = (bf, m)

infixr 3 <$$>
(<$$>) :: (a -> b) -> (a, c) -> (b, c)
(<$$>) f (a, c) = (f a, c)

insZero :: BFPointer -> BF3State -> BF3State
insZero q m = Map.insert q Zero m

insDontKnows :: [BFPointer] -> BF3State -> BF3State
insDontKnows q m = foldl (flip (`Map.insert` DontKnow)) m q

-- | Quick optimization for the head initialization.
dropHeadWhile :: BF3Code -> BF3Code
dropHeadWhile (Mult3 _ _ : bf)
  = dropHeadWhile bf
dropHeadWhile (While3 _ _ : bf)
  = dropHeadWhile bf
dropHeadWhile (If3 _ _ : bf)
  = dropHeadWhile bf
dropHeadWhile (Free3 _ : bf)
  = dropHeadWhile bf
dropHeadWhile bf = bf

initializeNear :: BF3Code -> BF3Code
initializeNear (AddI3 p n : Mult3 q r : bf)
  | p == q = let newp = head $ [ x | x <- nears, notElem x mults ]
                 nears = join $ map (\a -> [a - 1, a + 1]) mults
                 mults = map snd r
                 in AddI3 newp n : Mult3 newp r : bf
initializeNear bf = bf

-- | Quick optimization for make initialization lazy before neaded.
lazyInit :: BF3Code -> BF3Code
lazyInit (a@(AddI3 _ _) : bf)
  = a : lazyInit bf
lazyInit (m@(Mult3 _ _) : bf)
  = m : let (bfs, rest) = span isAddSubPut3 bf in sort' [] bfs ++ rest
  where
    sort' el (b@(AddI3 _ _) : bf') = sort' (el ++ [b]) bf'
    sort' el (b@(SubI3 _ _) : bf') = sort' (el ++ [b]) bf'
    sort' el (b@(Put3 q) : bf') = let (ads, rest) = classEq q el
                                   in ads ++ (b : sort' rest bf')
    sort' el bfs = el ++ bfs
    classEq q (b@(AddI3 p _) : bf') | p == q = (b:) <$$> classEq q bf'
    classEq q (b@(SubI3 p _) : bf') | p == q = (b:) <$$> classEq q bf'
    classEq q (b:bf') = (b:) <$> classEq q bf'
    classEq _ [] = ([], [])
lazyInit bf = bf

isFree3 :: BF3 -> Bool
isFree3 (Free3 _) = True
isFree3 _ = False

isAddSubPut3 :: BF3 -> Bool
isAddSubPut3 (AddI3 _ _) = True
isAddSubPut3 (SubI3 _ _) = True
isAddSubPut3 (Put3 _) = True
isAddSubPut3 _ = False

convert23 :: BF2Code -> BF3Code
convert23 = convert23'

convert23' :: BF2Code -> BF3Code
convert23' b@(Incr2 p : _) = let (bf, bfs) = span (sameIncr2 p) b
                                  in AddI3 p (length bf) : convert23' bfs
convert23' b@(Decr2 p : _) = let (bf, bfs) = span (sameDecr2 p) b
                                 in SubI3 p (length bf) : convert23' bfs
convert23' (While2 p [ Decr2 q ] : bf) | p == q = Free3 p : convert23' bf
convert23' (While2 p bf' : bf)
  | all isIncr2OrDecr2 bf' && existOne (Decr2 p) bf' && Incr2 p `notElem` bf'
  = Mult3 p (map toTuple (convert23' (filter (/=Decr2 p) bf'))) : convert23' bf
  where
    toTuple (AddI3 r i) = (i, r)
    toTuple (SubI3 r i) = (-i, r)
    toTuple _ = (0, -1)
convert23' (While2 p bf' : bf) = While3 p (convert23' bf') : convert23' bf
convert23' (Put2 p : bf) = Put3 p : convert23' bf
convert23' (Get2 p : bf) = Get3 p : convert23' bf
convert23' (Raw2 b p q : bf) = Raw3 b p q : convert23' bf
convert23' [] = []

sameIncr2 :: BFPointer -> BF2 -> Bool
sameIncr2 p (Incr2 q) = p == q
sameIncr2 _ _ = False

sameDecr2 :: BFPointer -> BF2 -> Bool
sameDecr2 p (Decr2 q) = p == q
sameDecr2 _ _ = False

isIncr2OrDecr2 :: BF2 -> Bool
isIncr2OrDecr2 (Incr2 _) = True
isIncr2OrDecr2 (Decr2 _) = True
isIncr2OrDecr2 _ = False

