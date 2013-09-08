module Language.Brainfuck.C2BF.BF5
  ( BF5Code
  , BF5 (..)
  , BF5Var (..)
  , BF5Type (..)
  , typeof5
  , convert54
  , convert54Option
  -- , convert45
  )
  where
import Codec.Binary.UTF8.String (encode)
import Control.Monad.Instances
import Data.Functor ((<$>))
import Data.List (find, unfoldr)
import Data.Tuple (swap)
import qualified Data.Map.Lazy as Map
import Language.Brainfuck.C2BF.BF1
import Language.Brainfuck.C2BF.BF4

-- | Layer 5 Brainfuck
--
-- This layer deals with C-like variables, char, short, long and long long and
-- their signed versions.  If you use this layer, you cannot touch the memory
-- address, which is completely concealed.
type BF5Code = [BF5]
data BF5 = Init5 Integer (BF5Var -> BF5Code)
         | InitChar5 Integer (BF5Var -> BF5Code)
         | InitShort5 Integer (BF5Var -> BF5Code)
         | InitLong5 Integer (BF5Var -> BF5Code)
         | InitLongLong5 Integer (BF5Var -> BF5Code)
         | AddI5 BF5Var Integer
         | SubI5 BF5Var Integer
         | AddVar5 BF5Var BF5Var
         | SubVar5 BF5Var BF5Var
         | MoveAdd5 BF5Var BF5Var
         | MoveSub5 BF5Var BF5Var
         | MulVar5 BF5Var BF5Var (BF5Var -> BF5Code)
         | MulI5 BF5Var BFInt (BF5Var -> BF5Code)
         | Assign5 BF5Var BF5Var
         | AssignI5 BF5Var BFInt
         | PrintS5 String
         | PrintD5 BF5Var
         | Put5 BF5Var
         | While5 BF5Var BF5Code
         | If5 BF5Var BF5Code
         | IfPos5 BF5Var BF5Code
         | Get5 BF5Var
         | Free5 BF5Var

type BF5State = (Map.Map BF5Var [BFPointer], BF5Id)

type BF5Id = Int

data BF5Var = BF5Char BF5Id
            | BF5Short BF5Id
            | BF5Long BF5Id
            | BF5LongLong BF5Id
            deriving (Eq, Ord, Show)

data BF5Type = BF5CharT
             | BF5ShortT
             | BF5LongT
             | BF5LongLongT
             deriving (Eq, Show)

instance BF BF5 where
  convertOption opt = convertOption opt . convert54Option opt
  upconvert = undefined -- convert45 . upconvert

initialState5 :: BF5State
initialState5 = (Map.empty, 0)

divMods :: Integer -> [BFInt]
divMods = unfoldr (\x -> Just (swap $ fromEnum <$> divMod x 256))

convert54Option :: [Option] -> BF5Code -> BF4Code
convert54Option _ = flip convert54' initialState5

convert54 :: BF5Code -> BF4Code
convert54 = convert54Option defaultOption

convert54' :: BF5Code -> BF5State -> BF4Code
convert54' (Init5 n cb : bf) mi
  = convert54' (InitChar5 n cb : bf) mi
convert54' (InitChar5 n cb : bf) (m, i)
  = let n' = BF5Char i
        i' = i + 1
        n0:_ = divMods n
        in Init4 n0 (\p -> let m' = (Map.insert n' [p] m, i')
                               in convert54' (cb n') m')
            : convert54' bf (m, i')
convert54' (InitShort5 n cb : bf) (m, i)
  = let n' = BF5Short i
        i' = i + 1
        n0:n1:_ = divMods n
        in ( Init4 n0 $ \p0 ->
           [ Init4 n1 $ \p1 -> let m' = (Map.insert n' [p0, p1] m, i')
                                   in convert54' (cb n') m' ])
            : convert54' bf (m, i')
convert54' (InitLong5 n cb : bf) (m, i)
  = let n' = BF5Long i
        i' = i + 1
        n0:n1:n2:n3:_ = divMods n
        in ( Init4 n0 $ \p0 ->
           [ Init4 n1 $ \p1 ->
           [ Init4 n2 $ \p2 ->
           [ Init4 n3 $ \p3 -> let m' = (Map.insert n' [p0, p1, p2, p3] m, i')
                                 in convert54' (cb n') m' ] ] ] )
            : convert54' bf (m, i')
convert54' (InitLongLong5 n cb : bf) (m, i)
  = let n' = BF5Long i
        i' = i + 1
        n0:n1:n2:n3:n4:n5:n6:n7:_ = divMods n
        in ( Init4 n0 $ \p0 ->
           [ Init4 n1 $ \p1 ->
           [ Init4 n2 $ \p2 ->
           [ Init4 n3 $ \p3 ->
           [ Init4 n4 $ \p4 ->
           [ Init4 n5 $ \p5 ->
           [ Init4 n6 $ \p6 ->
           [ Init4 n7 $ \p7 -> let m' = (Map.insert n' [p0, p1, p2, p3, p4, p5, p6, p7] m, i')
                                 in convert54' (cb n') m' ] ] ] ] ] ] ] )
            : convert54' bf (m, i')
convert54' (AddI5 c n : bf) mi
  | n < 0 = convert54' (SubI5 c (-n) : bf) mi
convert54' (AddI5 c n : bf) mi@(m, _)
  = case Map.lookup c m of
         Just [p] -> AddI4 p (fromEnum $ mod n 256) : convert54' bf mi
         Just [_, _] ->
           convert54' ( InitShort5 n (\n' -> [ MoveAdd5 n' c, Free5 n' ] ) : bf) mi
         Just [_, _, _, _] ->
           convert54' ( InitLong5 n (\n' -> [ MoveAdd5 n' c, Free5 n' ] ) : bf) mi
         Just [_, _, _, _, _, _, _, _] ->
           convert54' ( InitLongLong5 n (\n' -> [ MoveAdd5 n' c, Free5 n' ] ) : bf) mi
         _ -> varNotFound5 "AddI5" c
convert54' (SubI5 c n : bf) mi
  | n < 0 = convert54' (AddI5 c (-n) : bf) mi
convert54' (SubI5 c n : bf) mi@(m, _)
  = case Map.lookup c m of
         Just [p] -> SubI4 p (fromEnum $ mod n 256) : convert54' bf mi
         Just [_, _] ->
           convert54' ( InitShort5 n (\n' -> [ MoveSub5 n' c, Free5 n' ] ) : bf) mi
         Just [_, _, _, _] ->
           convert54' ( InitLong5 n (\n' -> [ MoveSub5 n' c, Free5 n' ] ) : bf) mi
         Just [_, _, _, _, _, _, _, _] ->
           convert54' ( InitLongLong5 n (\n' -> [ MoveSub5 n' c, Free5 n' ] ) : bf) mi
         _ -> varNotFound5 "SubI5" c
convert54' (MoveAdd5 n n' : bf) mi@(m, _)
  | typeof5 n == typeof5 n'
  = case Map.lookup n m of
         Just [p]
           -> case Map.lookup n' m of
                   Just [q] -- -> MoveAdd4 p [q] : convert54' bf mi
                      -> Init4 255 (\t ->  -- TODO
                         [ Sub4 q [t]
                         , IfGtElse4 p t
                           [ MoveAdd4 p [q] ]
                           [ Init4 255 (\u -> 
                             [ MoveSub4 p [u], MoveSub4 u [q], q -= 1]) ] ])
                                   : convert54' bf mi
                   _ -> varNotFound5 "MoveAdd5" n'
         Just [p0, p1]
           -> case Map.lookup n' m of
                    Just [q0, q1]
                      -> Init4 255 (\t ->  -- TODO bff がintなら，int用のコードはけばいいのでは
                         [ Sub4 q0 [t]
                         , IfGtElse4 p0 t
                           [ q1 += 1, MoveAdd4 p0 [q0] ]
                           [ Init4 255 (\u -> 
                             [ MoveSub4 p0 [u], MoveSub4 u [q0], q0 -= 1]) ] ])
                      -- -> While4 p0 [ p0 -= 1, q0 += 1, IfNot4 q0 [ q1 += 1 ] ]
                       : MoveAdd4 p1 [ q1 ]
                       : convert54' bf mi
                    _ -> varNotFound5 "MoveAdd5" n'
         Just [p0, p1, p2, p3]
           -> case Map.lookup n' m of
                    Just [q0, q1, q2, q3]
                      -> Init4 255 (\t ->  -- TODO
                         [ Sub4 q0 [t]
                         , IfGtElse4 p0 t
                           [ q1 += 1, MoveAdd4 p0 [q0] ]
                           [ Init4 255 (\u -> 
                             [ MoveSub4 p0 [u], Sub4 u [q0], q0 -= 1]) ] ])
                       : Init4 255 (\t ->
                         [ Sub4 q1 [t]
                         , IfGtElse4 p1 t
                           [ q2 += 1, MoveAdd4 p1 [q1] ]
                           [ Init4 255 (\u -> 
                             [ MoveSub4 p1 [u], Sub4 u [q1], q1 -= 1]) ] ])
                       : Init4 255 (\t ->
                         [ Sub4 q2 [t]
                         , IfGtElse4 p2 t
                           [ q3 += 1, MoveAdd4 p2 [q2] ]
                           [ Init4 255 (\u -> 
                             [ MoveSub4 p2 [u], Sub4 u [q2], q2 -= 1]) ] ])
                      -- -> While4 p0 [ p0 -= 1, q0 += 1, IfNot4 q0 [ q1 += 1 ] ]
                       -- : While4 p1 [ p1 -= 1, q1 += 1, IfNot4 q1 [ q2 += 1 ] ]
                       -- : While4 p2 [ p2 -= 1, q2 += 1, IfNot4 q2 [ q3 += 1 ] ]
                       : MoveAdd4 p3 [ q3 ]
                       : convert54' bf mi
                    _ -> varNotFound5 "MoveAdd5" n'
         Just [p0, p1, p2, p3, p4, p5, p6, p7]
           -> case Map.lookup n' m of
                    Just [q0, q1, q2, q3, q4, q5, q6, q7]
                      -> Init4 255 (\t ->
                         [ Sub4 q0 [t]
                         , IfGtElse4 p0 t [ q1 += 1, Add4 p0 [q0] ]
                                          [ Init4 255 (\u -> 
                                            [ Sub4 p0 [u], Sub4 u [q0], q0 -= 1]) ] ])
                      -- -> While4 p0 [ p0 -= 1, q0 += 1, IfNot4 q0 [ q1 += 1 ] ] -- TODO: assuming wrapping
                       : While4 p1 [ p1 -= 1, q1 += 1, IfNot4 q1 [ q2 += 1 ] ]
                       : While4 p2 [ p2 -= 1, q2 += 1, IfNot4 q2 [ q3 += 1 ] ]
                       : While4 p3 [ p3 -= 1, q3 += 1, IfNot4 q3 [ q4 += 1 ] ]
                       : While4 p4 [ p4 -= 1, q4 += 1, IfNot4 q4 [ q5 += 1 ] ]
                       : While4 p5 [ p5 -= 1, q5 += 1, IfNot4 q5 [ q6 += 1 ] ]
                       : While4 p6 [ p6 -= 1, q6 += 1, IfNot4 q6 [ q7 += 1 ] ]
                       : MoveAdd4 p7 [ q7 ]
                       : convert54' bf mi
                    _ -> varNotFound5 "MoveAdd5" n'
         _ -> varNotFound5 "MoveAdd5" n
  | otherwise = typeError5 "MoveAdd5" n n'
convert54' (While5 var bfs : bf) mi@(m, _)
  = case Map.lookup var m of
         Just [p] -> While4 p (convert54' bfs mi) : convert54' bf mi
         Just [p0, p1] ->
           Init4 1 (\t ->
             [ While4 t (IfNot4 p0 [ IfNot4 p1 [ t -= 1 ] ] : convert54' bfs mi) ])
               : convert54' bf mi
         _ -> varNotFound5 "While5" var
convert54' (If5 var bfs : bf) mi@(m, _)
  = case Map.lookup var m of
         Just [p] -> If4 p (convert54' bfs mi) : convert54' bf mi
         _ -> varNotFound5 "If5" var
convert54' (IfPos5 var bfs : bf) mi@(m, _)
  = case Map.lookup var m of
         Just [p] -> IfPos4' p (convert54' bfs mi) : convert54' bf mi
         _ -> varNotFound5 "IfPos5" var
convert54' (Assign5 n n' : bf) mi@(m, _)
  | typeof5 n == typeof5 n'
  = case Map.lookup n m of
         Just [p]
           -> case Map.lookup n' m of
                   Just [q]
                     -> AssignZero4 p
                      : Copy4 q (\t -> [MoveAdd4 t [p]])
                      : convert54' bf mi
                   _ -> varNotFound5 "Assign5" n'
         Just [p0, p1]
           -> case Map.lookup n' m of
                   Just [q0, q1]
                     -> AssignZero4 p0
                      : AssignZero4 p1
                      : Copy4 q0 (\t -> [MoveAdd4 t [p0]])
                      : Copy4 q1 (\t -> [MoveAdd4 t [p1]])
                      : convert54' bf mi
                   _ -> varNotFound5 "Assign5" n'
         Just [p0, p1, p2, p3]
           -> case Map.lookup n' m of
                   Just [q0, q1, q2, q3]
                     -> AssignZero4 p0
                      : AssignZero4 p1
                      : AssignZero4 p2
                      : AssignZero4 p3
                      : Copy4 q0 (\t -> [MoveAdd4 t [p0]])
                      : Copy4 q1 (\t -> [MoveAdd4 t [p1]])
                      : Copy4 q2 (\t -> [MoveAdd4 t [p2]])
                      : Copy4 q3 (\t -> [MoveAdd4 t [p3]])
                      : convert54' bf mi
                   _ -> varNotFound5 "Assign5" n'
         Just [p0, p1, p2, p3, p4, p5, p6, p7]
           -> case Map.lookup n' m of
                   Just [q0, q1, q2, q3, q4, q5, q6, q7]
                     -> AssignZero4 p0
                      : AssignZero4 p1
                      : AssignZero4 p2
                      : AssignZero4 p3
                      : AssignZero4 p4
                      : AssignZero4 p5
                      : AssignZero4 p6
                      : AssignZero4 p7
                      : Copy4 q0 (\t -> [MoveAdd4 t [p0]])
                      : Copy4 q1 (\t -> [MoveAdd4 t [p1]])
                      : Copy4 q2 (\t -> [MoveAdd4 t [p2]])
                      : Copy4 q3 (\t -> [MoveAdd4 t [p3]])
                      : Copy4 q4 (\t -> [MoveAdd4 t [p4]])
                      : Copy4 q5 (\t -> [MoveAdd4 t [p5]])
                      : Copy4 q6 (\t -> [MoveAdd4 t [p6]])
                      : Copy4 q7 (\t -> [MoveAdd4 t [p7]])
                      : convert54' bf mi
                   _ -> varNotFound5 "Assign5" n'
         _ -> varNotFound5 "Assign5" n'
  | otherwise = typeError5 "Assign5" n n'
convert54' (Put5 var : bf) mi@(m, _)
  = case Map.lookup var m of
         Just [p] -> Put4 p  : convert54' bf mi
         Just [p0, p1] -> Put4 p0 : convert54' bf mi -- TODO: ???
         _ -> varNotFound5 "Put5" var
convert54' (Get5 var : bf) mi@(m, _)
  = case Map.lookup var m of
         Just [p] -> Get4 p  : convert54' bf mi
         Just [p0, p1] -> Get4 p0 : convert54' bf mi -- TODO: scanf???
         _ -> varNotFound5 "Get5" var
convert54' (p@(PrintS5 _) : c@(InitChar5 _ _) : bf) (m, i)
  = convert54' (c : p : bf) (m, i)
convert54' (PrintS5 cs : PrintS5 bs : bf) (m, i)
  = convert54' (PrintS5 (cs ++ bs) : bf) (m, i)
convert54' (PrintS5 cs : bf) (m, i)
  = let (ss, _, ns) = classify cs in printgo ss ns [] ++ convert54' bf (m, i)
  where
    printgo [] [] _
      = [] -- convert54' bf (m, i)
    printgo ((s, j):ss) [] xs
      = let x = xs !! j in AddI4 x s : Put4 x : printgo ss [] xs
    printgo ss ((mm, _):ms) xs
      = [Init4 mm (\x -> (printgo ss ms (xs ++ [x]) ++ [Free4 x]))]

convert54' (PrintD5 var : bf) mi@(m, _)
  = case Map.lookup var m of
         Just [p]
           -> ModDiv4 p 10 (\m0 d0 ->
            [ ModDiv4' d0 10 (\m0' d0' ->
              let t = m0' - 1 in
                [ If4 d0' [ AddI4 d0' 48, Put4 d0', AddI4 t 1, AddI4 m0' 1 ]
                , Lock4 (m0' + 1)
                , If4 m0' [ MoveSub4 t [ m0' ], AddI4 m0' 48, Put4 m0' ]
                , Free4 (m0' + 1)
                , AddI4 m0 48, Put4 m0
                ]) ])
            : convert54' bf mi

         Just [p0, p1]
           -> ModDiv4 p1 10 (\m1 d1 ->      -- p1 = 10 * d1 + m1
            [ ModDiv4' d1 10 (\m1' d1' ->   -- p1 = 100 * d1' + 10 * m1' + m1
            [ ModDiv4 p0 10 (\m0 d0 ->      -- p0 = 10 * d0 + m0
            [ ModDiv4' d0 10 (\m0' d0' ->   -- p0 = 100 * d0' + 10 * m0' + m0
            [ Init4 0 (\a4 ->
            [ Init4 0 (\a3 ->
            let a2 = d0'; a1 = m0'; a0 = m0; in
            -- p1 * 256 + p0
            --        = (100 * d1' + 10 * m1' + m1) * 256
            --        + (100 * d0' + 10 * m0' + m0)
            --  (a4)  = (2 * d1') * 10000
            --  (a3)  + (5 * d1' + 2 * m1') * 1000
            --  (a2)  + (6 * d1' + 5 * m1' + 2 * m1 + d0') * 100
            --  (a1)  + (          6 * m1' + 5 * m1       + m0') * 10
            --  (a0)  + (                  + 6 * m1             + m0)
            [ Mult4 d1' [(2, a4), (5, a3), (6, a2)], Free4 d1'
            , Mult4 m1' [(2, a3), (5, a2), (6, a1)], Free4 m1'
            , Mult4 m1 [(2, a2), (5, a1), (6, a0)], Free4 m1
            -- , MoveAdd4 d0' [ a2 ], MoveAdd4 m0' [ a1 ], MoveAdd4 m0 [ a0 ]
            , Lock4 a4, Lock4 a3, Lock4 a2, Lock4 a1, Lock4 a0
            , Move4 a0
            , Init4 7 (\t -> -- (6 + 1) * 9 = 72
              [ While4 t [ SubI4 t 1, IfGtI4 a0 9 [ AddI4 a1 1, SubI4 a0 10 ] ] ])
            , Move4 a1
            , Init4 11 (\t -> -- (6 + 5 + 1) * 9 + 7 = 115
              [ While4 t [ SubI4 t 1, IfGtI4 a1 9 [ AddI4 a2 1, SubI4 a1 10 ] ] ])
            , Move4 a2
            , Init4 13 (\t -> -- (6 + 5 + 2 + 1) * 9 + 11 = 137
              [ While4 t [ SubI4 t 1, IfGtI4 a2 9 [ AddI4 a3 1, SubI4 a2 10 ] ] ])
            , Move4 a3
            , Init4 7 (\t -> -- (5 + 2) * 9 + 13 = 76
              [ While4 t [ SubI4 t 1, IfGtI4 a3 9 [ AddI4 a4 1, SubI4 a3 10 ] ] ])
            , Move4 a3
            , Init4 0 (\a3' ->
            [ Lock4 a3', Init4 2 (\t -> -- 2 * 9 + 7 = 25
              [ While4 t [ SubI4 t 1, IfGtI4 a4 9 [ SubI4 a4 10 ] ] ])
            , If4 a4 [ AddI4 a4 48, Put4 a4, AddI4 a3' 1, AddI4 a3 1 ]
            , Move4 a2
            , Init4 0 (\a2' ->
            [ Lock4 a2', If4 a3 [ AddI4 a3 48, MoveSub4 a3' [a3], Put4 a3
                           , AddI4 a2' 1, AddI4 a2 1 ]
            , Move4 a1
            , Init4 0 (\a1' ->
            [ Lock4 a1', If4 a2 [ AddI4 a2 48, MoveSub4 a2' [a2], Put4 a2
                           , AddI4 a1' 1, AddI4 a1 1 ]
            , If4 a1 [ AddI4 a1 48, MoveSub4 a1' [a1], Put4 a1 ], Free4 a1'
            , AddI4 a0 48, Put4 a0
            -- , Free4 a4, Free4 a3, Free4 a2, Free4 a1, Free4 a0
            -- , Free4 a3', Free4 a2'
            ]) ]) ]) ]) ]) ]) ]) ]) ])
            : convert54' bf mi

         Just [p0, p1, p2, p3]
           -> ModDiv4 p3 10 (\m1 d1 ->    -- p3 = 10 * d1 + m1
            [ ModDiv4' d1 10 (\m1' d1' -> -- p3 = 100 * d1' + 10 * m1' + m1
            [ ModDiv4 p2 10 (\m2 d2 ->    -- p2 = 10 * d2 + m2
            [ ModDiv4' d2 10 (\m2' d2' -> -- p2 = 100 * d2' + 10 * m2' + m2
            [ ModDiv4 p1 10 (\m3 d3 ->    -- p1 = 10 * d3 + m3
            [ ModDiv4' d3 10 (\m3' d3' -> -- p1 = 100 * d3' + 10 * m3' + m3
            [ ModDiv4 p0 10 (\m4 d4 ->    -- p0 = 10 * d4 + m4
            [ ModDiv4' d4 10 (\m4' d4' -> -- p0 = 100 * d4' + 10 * m4' + m4
            [ Init4 0 (\c1 ->
            [ Init4 0 (\c2 ->
            [ Init4 0 (\c3 ->
            [ Init4 0 (\c4 ->
            [ Init4 0 (\c5 ->
            [ Init4 0 (\c6 ->
            [ Init4 0 (\c7 ->
            let c8 = d4'; c9 = m4'; c10 = m4; in
            -- ((p3 * 256 + p2) * 256 + p1) * 256 + p0
            --       = (100 * d1' + 10 * m1' + m1) * 16777216
            --       + (100 * d2' + 10 * m2' + m2) * 65536
            --       + (100 * d3' + 10 * m3' + m3) * 256
            --       + (100 * d4' + 10 * m4' + m4)
            -- (c1)  = (1 * d1') * 1000000000
            -- (c2)  + (6 * d1' + 1 * m1') * 100000000
            -- (c3)  + (7 * d1' + 6 * m1' + 1 * m1) * 10000000
            -- (c4)  + (7 * d1' + 7 * m1' + 6 * m1 + 6 * d2') * 1000000
            -- (c5)  + (7 * d1' + 7 * m1' + 7 * m1 + 5 * d2' + 6 * m2') * 100000
            -- (c6)  + (2 * d1' + 7 * m1' + 7 * m1 + 5 * d2' + 5 * m2' + 6 * m2 + 2 * d3') * 10000
            -- (c7)  + (1 * d1' + 2 * m1' + 7 * m1 + 3 * d2' + 5 * m2' + 5 * m2 + 5 * d3' + 2 * m3') * 1000
            -- (c8)  + (6 * d1' + 1 * m1' + 2 * m1 + 6 * d2' + 3 * m2' + 5 * m2 + 6 * d3' + 5 * m3' + 2 * m3 + d4') * 100
            -- (c9)  + (          6 * m1' + 1 * m1           + 6 * m2' + 3 * m2           + 6 * m3' + 5 * m3 + m4') * 10
            -- (c10) + (                    6 * m1                     + 6 * m2                     + 6 * m3 + m4) * 1
            [ Mult4 d1' [(1, c1), (6, c2), (7, c3), (7, c4), (7, c5), (2, c6), (1, c7), (6, c8)], Free4 d1'
            , Mult4 m1' [(1, c2), (6, c3), (7, c4), (7, c5), (7, c6), (2, c7), (1, c8), (6, c9)], Free4 m1'
            , Mult4 m1 [(1, c3), (6, c4), (7, c5), (7, c6), (7, c7), (2, c8), (1, c9), (6, c10)], Free4 m1
            , Mult4 d2' [(6, c4), (5, c5), (5, c6), (3, c7), (6, c8)], Free4 d2'
            , Mult4 m2' [(6, c5), (5, c6), (5, c7), (3, c8), (6, c9)], Free4 m2'
            , Mult4 m2 [(6, c6), (5, c7), (5, c8), (3, c9), (6, c10)], Free4 m2
            , Mult4 d3' [(2, c6), (5, c7), (6, c8)], Free4 d3'
            , Mult4 m3' [(2, c7), (5, c8), (6, c9)], Free4 m3'
            , Mult4 m3 [(2, c8), (5, c9), (6, c10)], Free4 m3
            -- , MoveAdd4 d4' [ c8 ], MoveAdd4 m4' [ c10 ], MoveAdd4 m4 [ c10 ]
            , Lock4 c1, Lock4 c2, Lock4 c3, Lock4 c4, Lock4 c5
            , Lock4 c6, Lock4 c7, Lock4 c8, Lock4 c9, Lock4 c10
            , Move4 c10
            , Init4 17 (\t -> -- (6 + 6 + 6 + 1) * 9 = 171
              [ While4 t [ SubI4 t 1, IfGtI4 c10 9 [ AddI4 c9 1, SubI4 c10 10 ] ] ])
            , Move4 c9
            , Init4 26 (\t -> -- (6 + 1 + 6 + 3 + 6 + 5 + 1) * 9 + 17 = 269 -- TODO: あかん: 256よりでかいやん
              [ While4 t [ SubI4 t 1, IfGtI4 c9 9 [ AddI4 c8 1, SubI4 c9 10 ] ] ])
            , Move4 c8
            , Init4 35 (\t -> -- (6 + 1 + 2 + 6 + 3 + 5 + 6 + 5 + 2 + 1) * 9 + 26 = 359
              [ While4 t [ SubI4 t 1, IfGtI4 c8 9 [ AddI4 c7 1, SubI4 c8 10 ] ] ])
            , Move4 c7
            , Init4 30 (\t -> -- (1 + 2 + 7 + 3 + 5 + 5 + 5 + 2) * 9 + 35 = 305
              [ While4 t [ SubI4 t 1, IfGtI4 c7 9 [ AddI4 c6 1, SubI4 c7 10 ] ] ])
            , Move4 c6
            , Init4 33 (\t -> -- (2 + 7 + 7 + 5 + 5 + 6 + 2) * 9 + 30 = 336
              [ While4 t [ SubI4 t 1, IfGtI4 c6 9 [ AddI4 c5 1, SubI4 c6 10 ] ] ])
            , Move4 c5
            , Init4 32 (\t -> -- (7 + 7 + 7 + 5 + 6) * 9 + 33 = 321
              [ While4 t [ SubI4 t 1, IfGtI4 c5 9 [ AddI4 c4 1, SubI4 c5 10 ] ] ])
            , Move4 c4
            , Init4 26 (\t -> -- (7 + 7 + 6 + 6) * 9 + 32 = 266
              [ While4 t [ SubI4 t 1, IfGtI4 c4 9 [ AddI4 c3 1, SubI4 c4 10 ] ] ])
            , Move4 c3
            , Init4 15 (\t -> -- (7 + 6 + 1) * 9 + 26 = 152
              [ While4 t [ SubI4 t 1, IfGtI4 c3 9 [ AddI4 c2 1, SubI4 c3 10 ] ] ])
            , Move4 c2
            , Init4 7 (\t -> -- (6 + 1) * 9 + 15 = 78
              [ While4 t [ SubI4 t 1, IfGtI4 c2 9 [ AddI4 c1 1, SubI4 c2 10 ] ] ])
            , Move4 c1
            , Init4 1 (\t -> -- 1 * 9 + 7 = 16
              [ While4 t [ SubI4 t 1, IfGtI4 c1 9 [ SubI4 c1 10 ] ] ])
            , Move4 c2
            , Init4 0 (\c2' ->
            [ Lock4 c2', If4 c1 [ AddI4 c1 48, Put4 c1, AddI4 c2' 1, AddI4 c2 1 ]
            , Move4 c3
            , Init4 0 (\c3' ->
            [ Lock4 c3', If4 c2 [ AddI4 c2 48, MoveSub4 c2' [c2], Put4 c2, AddI4 c3' 1, AddI4 c3 1 ]
            , FreeZero4 c2, FreeZero4 c2'
            , Move4 c4
            , Init4 0 (\c4' ->
            [ Lock4 c4', If4 c3 [ AddI4 c3 48, MoveSub4 c3' [c3], Put4 c3, AddI4 c4' 1, AddI4 c4 1 ]
            , FreeZero4 c3, FreeZero4 c3'
            , Move4 c5
            , Init4 0 (\c5' ->
            [ Lock4 c5', If4 c4 [ AddI4 c4 48, MoveSub4 c4' [c4], Put4 c4, AddI4 c5' 1, AddI4 c5 1 ]
            , FreeZero4 c4, FreeZero4 c4'
            , Move4 c6
            , Init4 0 (\c6' ->
            [ Lock4 c6', If4 c5 [ AddI4 c5 48, MoveSub4 c5' [c5], Put4 c5, AddI4 c6' 1, AddI4 c6 1 ]
            , FreeZero4 c5, FreeZero4 c5'
            , Move4 c7
            , Init4 0 (\c7' ->
            [ Lock4 c7', If4 c6 [ AddI4 c6 48, MoveSub4 c6' [c6], Put4 c6, AddI4 c7' 1, AddI4 c7 1 ]
            , FreeZero4 c6, FreeZero4 c6'
            , Move4 c8
            , Init4 0 (\c8' ->
            [ Lock4 c8', If4 c7 [ AddI4 c7 48, MoveSub4 c7' [c7], Put4 c7, AddI4 c8' 1, AddI4 c8 1 ]
            , FreeZero4 c7, FreeZero4 c7'
            , Move4 c9
            , Init4 0 (\c9' ->
            [ Lock4 c9', If4 c8 [ AddI4 c8 48, MoveSub4 c8' [c8], Put4 c8, AddI4 c9' 1, AddI4 c9 1 ]
            , FreeZero4 c8, FreeZero4 c8'
            , If4 c9 [ AddI4 c9 48, MoveSub4 c9' [c9] , Put4 c9 ]
            , AddI4 c10 48, Put4 c10
            -- , Free4 c9', Free4 c8', Free4 c7', Free4 c6'
            -- , Free4 c5', Free4 c4', Free4 c3', Free4 c2'
            -- , Free4 c10, Free4 c9, Free4 c8, Free4 c7, Free4 c6
            -- , Free4 c5, Free4 c4, Free4 c3, Free4 c2, Free4 c1
            , FreeZero4 c9'
            ]), FreeZero4 c8'
            ]), FreeZero4 c7'
            ]), FreeZero4 c6'
            ]), FreeZero4 c5'
            ]), FreeZero4 c4'
            ]), FreeZero4 c3'
            ]), FreeZero4 c2'
            ]), FreeZero4 c7
            ]), FreeZero4 c6
            ]), FreeZero4 c5
            ]), FreeZero4 c4
            ]), FreeZero4 c3
            ]), FreeZero4 c2
            ]), FreeZero4 c1
            ])
            ])
            ]), FreeZero4 m3', FreeZero4 d3'
            ]), FreeZero4 m3, FreeZero4 d3
            ]), FreeZero4 m2', FreeZero4 d2'
            ]), FreeZero4 m2, FreeZero4 d2
            ]), FreeZero4 m1', FreeZero4 d1'
            ]), FreeZero4 m1, FreeZero4 d1
            ])
            : convert54' bf mi
         Just [p0, p1, p2, p3, p4, p5, p6, p7]
           -> undefined -- TODO
         _ -> varNotFound5 "PrintD5" var
convert54' [] _ = []
convert54' _ _ = undefined

classify :: String -> ([(Int, Int)], [(Int, Int)], [(Int, Int)])
classify cs = clgo (map fromEnum (encode cs)) [] [] [] 0
  where
    clgo [] ss ms ns _ = (ss, ms, ns)
    clgo (a:as) ss ms ns i
      = case find (\(x, _) -> a - 13 < x && x < a + 13) ms of
             Just (y, j) ->
               clgo as (ss ++ [(a - y, j)]) (repl ms j (a, j)) ns i
             Nothing ->
               clgo as (ss ++ [(0,i)]) (ms ++ [(a,i)]) (ns ++ [(a,i)]) (i + 1)

    repl :: [a] -> Int -> a -> [a]
    repl xs i x = take i xs ++ (x : drop (i + 1) xs)

typeof5 :: BF5Var -> BF5Type
typeof5 (BF5Char _) = BF5CharT
typeof5 (BF5Short _) = BF5ShortT
typeof5 (BF5Long _) = BF5LongT
typeof5 (BF5LongLong _) = BF5LongLongT

varNotFound5 :: Show a => String -> a -> t
varNotFound5 x y = error $ "Variable not found in " ++ x ++ ": " ++ show y

typeError5 :: Show a => String -> a -> a -> t
typeError5 x y z = error $ "Type error in " ++ x ++ ": " ++ show y ++ ", " ++ show z

