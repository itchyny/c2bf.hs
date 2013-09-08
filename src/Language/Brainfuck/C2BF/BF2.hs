{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Brainfuck.C2BF.BF2
  ( BF2Code
  , BF2 (..)
  , BF2State
  , convert21
  , convert21Option
  , convert12
  )
  where
import Language.Brainfuck.C2BF.BF0
import Language.Brainfuck.C2BF.BF1
import Data.Functor ((<$>))
import Data.List (delete)


-- | Layer 2 Brainfuck
--
-- This layer conceals codes for moving around the memory. It looks like very
-- primitive, but at this layer, some codes like [>], which is an actually valid
-- Brainfuck code, cannot be generated, due to abstraction of loop. However,
-- still it is Turing complete. And codes like [>] or [<] can be halmful, in a
-- sense that they can cause runtime crash (negative memory access) And code 
-- joining should be closed in the set of all safe codes (joining any safe 
-- codes should be safe).  The codes generated by BF2 can be easily checked its
-- safety, by counting the number of > and <, without running.
type BF2Code = [BF2]
data BF2 = Incr2 BFPointer
         | Decr2 BFPointer
         | Put2 BFPointer
         | Get2 BFPointer
         | While2 BFPointer BF2Code
-- But sometimes we want something halmful, from high layers.
-- First argument is raw level code, second arg is the position to start,
-- last arg is the resulting position at the end.
         | Raw2 BFCode BFPointer BFPointer
         deriving (Eq, Show)

type BF2State = BFPointer

initialState2 :: BF2State
initialState2 = 0

instance BF BF2 where
  convertOption opt = convertOption opt . convert21Option opt
  upconvert = convert12 . upconvert

instance BFRaw BF2 where
  raw = Raw2

convert21Option :: [Option] -> BF2Code -> BFCode
convert21Option opt
  | elem OffsetAddress opt
    = convert21Option (delete OffsetAddress opt) . offsetAddress
  | otherwise = snd . flip convert21' initialState2

convert21 :: BF2Code -> BFCode
convert21 = convert21Option defaultOption

convert21' :: BF2Code -> BF2State -> (BF2State, BFCode)
convert21' (Incr2 q : bf) p = (move p q++) <$> (Incr:) <$> convert21' bf q
convert21' (Decr2 q : bf) p = (move p q++) <$> (Decr:) <$> convert21' bf q
convert21' (Put2 q : bf) p = (move p q++) <$> (Put:) <$> convert21' bf q
convert21' (Get2 q : bf) p = (move p q++) <$> (Get:) <$> convert21' bf q
convert21' (w@(While2 q _) : While2 r _ : bf) p
  -- Optimization. Memory is asserted to be zero after first while
  | q == r = convert21' (w : bf) p
convert21' (While2 q bf' : bf) p
  = (\(p', c) -> (move p q++) <$> (While (c ++ move p' q):)
                              <$> convert21' bf q) (convert21' bf' q)
convert21' (Raw2 b p q : bf) r = (move r p++) <$> (b++) <$> convert21' bf q
convert21' [] p = (p, [])

move :: BF2State -> BF2State -> BFCode
move p q | p < q = replicate (fromEnum (q - p)) Next
         | p > q = replicate (fromEnum (p - q)) Prev
         | otherwise = []

data MinAddress = Undef | JustI Integer | Maximum deriving (Eq, Ord)
offsetAddress :: BF2Code -> BF2Code
offsetAddress bf = drop' (unJust (offset bf)) bf
  where
    unJust Undef = 0
    unJust (JustI i) = i
    unJust Maximum = 0
    pointer (Incr2 p) = JustI p
    pointer (Decr2 p) = JustI p
    pointer (Put2 p) = JustI p
    pointer (Get2 p) = JustI p
    pointer (While2 0 _) = JustI 0
    pointer (While2 p bfs) = minimum (JustI p : map pointer bfs)
    pointer (Raw2 _ _ _) = Undef
    offset (b : bfs) = let p = pointer b
                           in if p == JustI 0
                                 then JustI 0
                                 else minimum (p : map pointer bfs)
    offset _ = Maximum
    drop' 0 b = b
    drop' n (Incr2 p : b)
      = Incr2 (p - n) : drop' n b
    drop' n (Decr2 p : b)
      = Decr2 (p - n) : drop' n b
    drop' n (Put2 p : b)
      = Put2 (p - n) : drop' n b
    drop' n (Get2 p : b)
      = Get2 (p - n) : drop' n b
    drop' n (While2 p bfs : b)
      = While2 (p - n) (drop' n bfs) : drop' n b
    drop' n (Raw2 bfs p q : b)
      = Raw2 bfs (p - n) (q - n) : drop' n b
    drop' _ [] = []


-- convert12 . convert21 == id
-- should be satisfied anytime, but
-- convert21 . convert12 == id
-- is not always satisfied.
convert12 :: BFCode -> BF2Code
convert12 = snd . flip convert12' initialState2

convert12' :: BFCode -> BF2State -> (BF2State, BF2Code)
convert12' (Incr : bf) p = (Incr2 p:) <$> convert12' bf p
convert12' (Decr : bf) p = (Decr2 p:) <$> convert12' bf p
convert12' (Next : bf) p = convert12' bf (p + 1)
convert12' (Prev : bf) p = convert12' bf (p - 1)
convert12' (While [ Decr, Next, Incr, Next, Decr, While [ Next, Incr, Next,
  Next ], Next, While [ Incr, While [ Decr, Prev, Incr, Next ], Next, Incr,
  Next, Next ], Prev, Prev, Prev, Prev, Prev, Prev ] : bf) p
  = (rawModulo p:) <$> convert12' bf p
convert12' (While [ Decr, Next, Decr, While [ Next, Incr, Next, Next ],
  Next, While [ Incr, While [ Decr, Prev, Incr, Next ], Next, Incr, Next,
  Next ], Prev, Prev, Prev, Prev, Prev ] : bf) p
  = (rawModulo' p:) <$> convert12' bf p
convert12' bfs@(While bf : bf') p
  | countMoves bf == Just 0 = let (q, b) = convert12' bf p
                                  in (While2 p b:) <$> convert12' bf' q
  | otherwise = (p, [Raw2 bfs p p])
convert12' (Put : bf) p = (Put2 p:) <$> convert12' bf p
convert12' (Get : bf) p = (Get2 p:) <$> convert12' bf p
convert12' [] p = (p, [])

countMoves :: BFCode -> Maybe Int
countMoves (Next : bf) = (+1) <$> countMoves bf
countMoves (Prev : bf) = subtract 1 <$> countMoves bf
countMoves (While [ Decr, Next, Incr, Next, Decr, While [ Next, Incr, Next,
  Next ], Next, While [ Incr, While [ Decr, Prev, Incr, Next ], Next, Incr,
  Next, Next ], Prev, Prev, Prev, Prev, Prev, Prev ] : bf)
           = countMoves bf
countMoves (While [ Decr, Next, Decr, While [ Next, Incr, Next, Next ],
  Next, While [ Incr, While [ Decr, Prev, Incr, Next ], Next, Incr, Next,
  Next ], Prev, Prev, Prev, Prev, Prev ] : bf)
           = countMoves bf
countMoves (While bf : bf') | countMoves bf == Just 0 = countMoves bf'
                            | otherwise = Nothing
countMoves (_ : bf) = countMoves bf
countMoves [] = Just 0
