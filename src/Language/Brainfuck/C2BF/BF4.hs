module Language.Brainfuck.C2BF.BF4
  ( BF4Code
  , BF4 (..)
  , convert43
  , convert43Option
  -- , convert34
  )
  where
import Data.Functor ((<$>))
import Data.List (sortBy, nubBy)
import Data.Maybe (isNothing)
import qualified Data.Map.Lazy as Map
import Language.Brainfuck.C2BF.BF0
import Language.Brainfuck.C2BF.BF1
import Language.Brainfuck.C2BF.BF3
import Language.Brainfuck.C2BF.Util

-- | Layer 4 Brainfuck
--
-- This layer deals with variable automatic placement on the the memory.
-- And there are many rich instructions, which can results in longer codes.
-- However, memory safety is guaranteed.
type BF4Code = [BF4]
data BF4 = Init4 BFInt (BFPointer -> BF4Code)
         | MoveAdd4 BFPointer [BFPointer]
         | MoveSub4 BFPointer [BFPointer]
         | Add4 BFPointer [BFPointer]
         | Sub4 BFPointer [BFPointer]
         | AddI4 BFPointer BFInt
         | AddI4s [(BFPointer, BFInt)]
         | SubI4 BFPointer BFInt
         | MulI4 BFPointer BFInt
         | AssignI4 BFPointer BFInt
         | Copy4 BFPointer (BFPointer -> BF4Code)
         | Dupl4 BFPointer (BFPointer -> BFPointer -> BF4Code)
         | Mult4 BFPointer [(BFInt, BFPointer)]
         | MultTo4 BFPointer BFPointer
         | While4 BFPointer BF4Code
         | DoWhile4 BFPointer BF4Code 
         | WhileLt4 BFPointer BFPointer BF4Code
         | If4 BFPointer BF4Code
         | If4' BFPointer BF4Code
         | IfPos4' BFPointer BF4Code
         | Not4 BFPointer (BFPointer -> BF4Code)
         | IfNot4 BFPointer BF4Code
         | IfElse4 BFPointer BF4Code BF4Code
         | IfElseFree4 BFPointer BF4Code BF4Code
         | Gt4 BFPointer BFPointer (BFPointer -> BF4Code)
         | GtEq4 BFPointer BFPointer (BFPointer -> BF4Code)
         | IfGt4 BFPointer BFPointer BF4Code
         | IfGtEq4 BFPointer BFPointer BF4Code
         | IfGtI4 BFPointer BFInt BF4Code
         | IfLtI4 BFPointer BFInt BF4Code
         | IfGtElse4 BFPointer BFPointer BF4Code BF4Code
         | IfGtIElse4 BFPointer BFInt BF4Code BF4Code
         --  | IfEqual4 BFPointer BFPointer BF4Code
         --  | IfGreater4 BFPointer BFPointer BF4Code
         | ModDiv4 BFPointer BFInt (BFPointer -> BFPointer -> BF4Code)
         | ModDiv4' BFPointer BFInt (BFPointer -> BFPointer -> BF4Code)
         | ModDiv4'' BFPointer BFPointer (BFPointer -> BFPointer -> BF4Code)
         | Free4 BFPointer
         | AssignZero4 BFPointer
         | FreeZero4 BFPointer
         | Lock4 BFPointer
         | Put4 BFPointer        -- printf("%c", (char)c);
         | Get4 BFPointer
         | Raw4 BFCode BFPointer BFPointer -- code, start, end
         | Move4 BFPointer
         | Debug4 BFPointer

-- | For elimination of unnecessary free
data BF4Flag = Zero | DontKnow | Lock | NonZero deriving (Eq, Show)
type BF4State = (Map.Map BFPointer BF4Flag, BFPointer)

instance BF BF4 where
  convertOption opt = convertOption opt . convert43Option opt
  upconvert = undefined -- convert34 . unconvert

instance BFRich BF4 where
  put = Put4
  get = Get4
  (+=) = AddI4
  (-=) = SubI4
  (*=) = MulI4

instance BFRaw BF4 where
  raw = Raw4

instance Show BF4 where -- TODO
  showList = (++) . dropLastComma . snd . go 0
    where
      go m (Init4 n cb : bf)
        = let (m', res) = go (m + 1) (cb m)
              in (++) ("{" ++ varHead ++ show m ++ "=" ++ show n
                             ++ dropLastComma (',':res) ++ "},") <$> go m' bf
      go m (MoveAdd4 p q : bf) = f4 "MoveAdd4" p q <$> go m bf
      go m (MoveSub4 p q : bf) = f4 "MoveSub4" p q <$> go m bf
      go m (Add4 p q : bf) = f1 "Add4" p q <$> go m bf
      go m (Sub4 p q : bf) = f1 "Sub4" p q <$> go m bf
      go m (AddI4 p q : bf) = f1 "AddI4" p q <$> go m bf
      go m (SubI4 p q : bf) = f1 "SubI4" p q <$> go m bf
      go m (AddI4s p : bf) = f2 "AddI4s" p <$> go m bf
      go m (AssignI4 p q : bf) = f1 "AssignI4" p q <$> go m bf
      go m (Mult4 p q : bf) = f1 "Mult4" p q <$> go m bf
      go m (MultTo4 p q : bf) = f1 "MultTo4" p q <$> go m bf
      go m (Put4 p : bf) = f2 "Put4" p <$> go m bf
      go m (While4 p bfs : bf)
        = let (m', f) = f3 "While4" m p bfs in f <$> go m' bf
      go m (IfPos4' p bfs : bf)
        = let (m', f) = f3 "IfPos4'" m p bfs in f <$> go m' bf
      go m (Copy4 p cb : bf)
        = let (m', res) = go (m + 1) (cb m)
              in (++) ("{" ++ varHead ++ show m ++ "=" ++ varHead ++ show p
                             ++ "," ++ dropLastComma res ++ "},") <$> go m' bf
      go m (AssignZero4 p : bf) = (++) (varHead ++ show p ++ "=0;") <$> go m bf
      go _ _ = (-1, "")
      varHead = "#"
      f1 name p q
        = (++) $ name ++ " " ++ varHead ++ show p ++ " " ++ show q ++ ","
      f4 name p q
        = (++) $ name ++ " " ++ varHead ++ show p ++ " " ++ printVars q ++ ","
      f2 name p
        = (++) $ name ++ " " ++ varHead ++ show p ++ ","
      f3 name m p bfs
        = let (m', res) = go m bfs
              in (m', (++) $ name ++ " " ++ varHead ++ show p
                                     ++ " [" ++ dropLastComma res ++ "],")
      dropLastComma = dropLastWhile (==',')
      printVars p = "[" ++ concatMap (\x -> varHead ++ show x) p ++ "]"

initialState4 :: BF4State
initialState4 = (Map.empty, 0)

convert43Option :: [Option] -> BF4Code -> BF3Code
convert43Option _ = flip convert43' initialState4

convert43 :: BF4Code -> BF3Code
convert43 = convert43Option defaultOption

convert43' :: BF4Code -> BF4State -> BF3Code
convert43' b@(Init4 n cb : bf) (s, p)
  | null (cb 0) = convert43' bf (s, p)
  | otherwise = let q = if s == Map.empty && existLargeInit b && p == 0
                           then searchNear s (p + 1)
                           else searchNear s p
                    m = DontKnow
                    h = if n == 0 then id else (AddI4 q n:)
                    in convert43' (h (cb q) ++ [Free4 q]) (Map.insert q m s, q)
                       ++ convert43' bf (s, q)
  where
    existLargeInit (Init4 m c : _) = m > 14 || existLargeInit (c 0)
    existLargeInit _ = False
convert43' (MoveAdd4 p q : bf) (s, _)
  = let s' = foldl (flip (Map.adjust (const DontKnow))) s q
        in Mult3 p ((,) 1 <$> q)
         : convert43' bf (Map.adjust (const Zero) p s', p)
convert43' (Add4 p q : bf) (s, r)
  = convert43' (Copy4 p (\t -> [ MoveAdd4 t q ]) : bf) (s, r)
convert43' (Sub4 p q : bf) (s, r)
  = convert43' (Copy4 p (\t -> [ MoveSub4 t q ]) : bf) (s, r)
convert43' (MoveSub4 p q : bf) (s, _)
  = let s' = foldl (flip (Map.adjust (const DontKnow))) s q
        in Mult3 p ((,) (-1) <$> q)
         : convert43' bf (Map.adjust (const Zero) p s', p)
convert43' (AddI4 p n : bf) (s, _)
  = let s' = Map.insert p DontKnow s
        in case partconst n of
                Left m -> AddI3 p m : convert43' bf (s', p)
                _ -> convert43' (AddI4s [(p, n)] : bf) (s', p)
convert43' (AddI4s [] : bf) (s, p)
  = convert43' bf (s, p)
convert43' (AddI4s ps : SubI4 p n : bf) (s, _)
  = let (m, h) = if n == 0
                    then (Zero, (AddI4s ps:))
                    else (NonZero, (AddI4s (ps ++ [(p, -n)]):))
        in convert43' (h bf) (Map.insert p m s, p)
convert43' (AddI4s ps : AddI4 p n : bf) (s, _)
  = let (m, h) = if n == 0
                    then (Zero, (AddI4s ps:))
                    else (NonZero, (AddI4s (ps ++ [(p, n)]):))
        in convert43' (h bf) (Map.insert p m s, p)
convert43' (AddI4s ps : Init4 n cb : bf) (s, _)
  = let q = searchNear s (fst (last ps)) 
        m = DontKnow -- TODO
        h = if n == 0 then (AddI4s ps:) else (AddI4s (ps ++ [(q, n)]):)
        in convert43' (h (cb q) ++ [ Free4 q ]) (Map.insert q m s, q)
        ++ convert43' bf (s, q)
convert43' (AddI4s ps : bf) (s, _) -- split if the positions are too far
  = let fs = multtogether (snd <$> ps)
        n = (\(a, _, _) -> a) (head fs)
        s' = foldl (flip (`Map.insert` NonZero)) s (fst <$> ps)
        p' = fst (head ps)
        q = searchNearT s' p'
        r = zip ((\(_, b, _) -> b) <$> fs) (fst <$> ps)
        sorter (AddI4 a _) (AddI4 b _) = if a > b then GT else LT
        t = sortBy sorter
          $ zipWith AddI4 (fst <$> ps) ((\(_, _, c) -> c) <$> fs)
        in convert43' (AddI4 q n : Mult4 q r : (t ++ bf)) (s', q)
convert43' (SubI4 p n' : bf) (s, _)
  = let s' = Map.adjust (const DontKnow) p s
        in convert43' (AddI4 p (-n') : bf) (s', p)
convert43' (MulI4 p n : bf) (s, _)
  = let s' = Map.adjust (const DontKnow) p s
        in convert43' (Init4 n (\t -> [ Init4 0 (\u -> [ Init4 0 (\v ->
                        [ MoveAdd4 p [ u ]
                        , While4 u
                          [ While4 t [ p += 1, v += 1, t -= 1 ]
                          , While4 v [ t += 1, v -= 1 ], u -= 1 ]
                        , Free4 t, Free4 u, Free4 v ]) ]) ]) : bf) (s', p)
convert43' (AssignI4 p n : bf) (s, _)
  = let s' = Map.adjust (const (if n == 0 then Zero else DontKnow)) p s
        in Free3 p : convert43' (AddI4 p n : bf) (s', p)
convert43' (Copy4 p cb : bf) (s, p')
  = let q = searchNear s p
        su = Map.insert q DontKnow s
        r = searchNearT su q
        sv = Map.insert r DontKnow su
        in convert43' ( MoveAdd4 p [ q, r ]
                      : MoveAdd4 r [ p ]
                      : Free4 r
                      : cb q
                      ++ [ Free4 q ]) (sv, p')
           ++ convert43' bf (s, p)
convert43' (Dupl4 p cb : bf) (s, _)
  = let q = searchNear s p
        su = Map.insert q DontKnow s
        r = searchNear su q
        sv = Map.insert r DontKnow su
        in convert43' ( MoveAdd4 p [ q, r ]
                      : cb q r ++ [ Free4 q, Free4 r ]) (sv, p)
           ++ convert43' bf (s, p)
convert43' (Mult4 p q : bf) (s, _)
  = let s' = foldl (flip (Map.adjust (const DontKnow))) s (snd <$> q)
        in Mult3 p q : convert43' bf (Map.adjust (const Zero) p s', p)
convert43' (MultTo4 p q : bf) (s, r)
  = convert43' ( Init4 0 (\t ->
               [ Init4 0 (\w ->
               [ MoveAdd4 q [t]
               , Copy4 p (\u ->
               [ While4 t
                 [ t -= 1
                 , MoveAdd4 u [w, q]
                 , MoveAdd4 w [u]
                 ] ]) ]) ]) : bf) (s, r)
convert43' (While4 p bfs : bf) (s, _)
  = let s' = Map.adjust (const Zero) p s
        in While3 p (convert43' bfs (s, p)) : convert43' bf (s', p)
convert43' (WhileLt4 p q bfs : bf) (s, r)
  = convert43' ( Init4 1 (\t ->
               [ While4 t
                 ( IfGt4 q p [SubI4 t 1] : bfs ), Free4 t ]) : bf) (s, r)
convert43' (If4 p bfs : bf) (s, _)
  = let s' = Map.adjust (const Zero) p s
        in If3 p (convert43' bfs (s, p)) : convert43' bf (s', p)
convert43' (If4' p bfs : bf) (s, _)
  = convert43' [Copy4 p (\t -> If4 t bfs : Free4 t : bf)] (s, p)
convert43' (IfPos4' p bfs : bf) (s, _)
  = convert43' [Copy4 p (\t ->
      SubI4 t 1 : If4 t bfs : Free4 t : bf)] (s, p)
convert43' (Not4 p cb : bf) (s, _)
  = convert43' (Copy4 p (\t -> -- TODO: DontKnow
      [ Init4 1 (\u -> If4 t [ Free4 u ] : cb u
                       ++ [ Free4 u, Free4 t ]) ]) : bf) (s, p)
convert43' (IfElse4 p bf1 bf2 : bf) (s, _)
  = convert43' (Copy4 p (\t ->
      [ Init4 1 (\u ->
        [ If4 t ((u -= 1) : bf1), If4 u bf2, Free4 u ])
                                              , Free4 t ]) : bf) (s, p)
convert43' (IfElseFree4 p bf1 bf2 : bf) (s, _)
  = convert43' ( Init4 1 (\u ->
               [ If4 p ((u -= 1) : bf1), If4 u bf2, Free4 u, Free4 p ])
                                                      : bf) (s, p)
convert43' (IfNot4 p bfs : bf) (s, _)
  = convert43' (Not4 p (\t -> [ If4 t bfs ]) : bf) (s, p)
convert43' (Gt4 p q cb : bf) (s, _)
  = convert43' (Copy4 p (\p' ->
     [ Init4 0 (\t ->
       Copy4 q (\q' ->
         [ While4 p' [ p' -= 1, IfElse4 q' [ q' -= 1 ] [ t += 1, Free4 p' ] ] ])
       : cb t) ]) : bf) (s, p)
convert43' (GtEq4 p q cb : bf) (s, r)
  = convert43' (Copy4 p (\p' ->
     [ Init4 0 (\t ->
       Copy4 q (\q' ->
         [ p' += 1
         , While4 p'
           [ p' -= 1, IfElse4 q' [ q' -= 1 ] [ t += 1, Free4 p' ] ]
         ]) : cb t) ]) : bf) (s, r)
convert43' (IfGt4 p q bfs : bf) (s, _)
  = convert43' (Gt4 p q (\t -> [ If4 t bfs, Free4 t ]) : bf) (s, p)
convert43' (IfGtEq4 p q bfs : bf) (s, r)
  = convert43' (GtEq4 p q (\t -> [ If4 t bfs, Free4 t ]) : bf) (s, r)
convert43' (IfGtI4 p n bfs : bf) (s, r)
  = convert43' (Copy4 p (\p' ->
     [ Init4 0 (\t ->
       [ Init4 n (\q' ->
         [ While4 p'
           [ p' -= 1, IfElse4 q' [ q' -= 1 ] [ t += 1, Free4 p' ] ]
         ])
         , If4 t bfs ]) ]) : bf) (s, r)
convert43' (IfLtI4 p n bfs : bf) (s, _)
  = convert43' (Copy4 p (\p' ->
     [ Init4 0 (\t ->
       [ Init4 n (\q' ->
         [ While4 p'
           [ p' -= 1, IfElse4 q' [ q' -= 1 ] [ t += 1, Free4 p' ] ]
         , Free4 q' ])
         , IfNot4 t bfs, Free4 t ]), Free4 p' ]) : bf) (s, p)
convert43' (IfGtElse4 p q bf1 bf2 : bf) (s, _)
  = convert43' (Gt4 p q (\t -> [ IfElseFree4 t bf1 bf2, Free4 t ]) : bf) (s, p)
convert43' (IfGtIElse4 p n bf1 bf2 : bf) (s, _)
  = convert43' (Init4 n (\q ->
               [ IfGtElse4 p q bf1 bf2, Free4 q ]) : bf) (s, p)
convert43' (Move4 p : bf) (s, _)
  = convert43' bf (s, p)
convert43' (Lock4 p : bf) (s, q)
  = convert43' bf (Map.insert p Lock s, q)
convert43' (Put4 p : bf) (s, _)
  = Put3 p : convert43' bf (s, p)
convert43' (Get4 p : bf) (s, _)
  = Get3 p : convert43' bf (s, p)
convert43' (ModDiv4 p i bfs : bf) (s, _)
  = let q = requireBlock (Map.delete p s) p 7
        s' = foldl (\u x -> Map.insert x DontKnow u) s [q+2,q+3,q+4]
        in convert43'
            ( MoveAdd4 p [ q ]
            : AddI4 (q + 2) i
            : rawModulo q
            : MoveAdd4 (q + 1) [ p ]
            : Free4 (q + 2)
            : Lock4 (q + 3)
            : Lock4 (q + 4)
            : bfs (q + 3) (q + 4)
            ++ ( Free4 (q + 3)
               : Free4 (q + 4) : bf)) (s', p)
convert43' (ModDiv4' p i bfs : bf) (s, _)
  = let q = requireBlock (Map.delete p s) p 6
        s' = foldl (\u x -> Map.insert x DontKnow u) s [q+1,q+2,q+3]
        in convert43'
            ( MoveAdd4 p [ q ]
            : AddI4 (q + 1) i
            : rawModulo' q
            : Free4 (q + 0)
            : Free4 (q + 1)
            : Lock4 (q + 2)
            : Lock4 (q + 3)
            : bfs (q + 2) (q + 3)
            ++ ( Free4 (q + 2)
               : Free4 (q + 3)
               : bf)) (s', p)
convert43' (ModDiv4'' p r bfs : bf) (s, _)
  = let q = requireBlock (Map.delete p s) p 7
        s' = foldl (\u x -> Map.insert x DontKnow u) s [q+2,q+3,q+4]
        in convert43'
            ( MoveAdd4 p [ q ]
            : MoveAdd4 r [q + 1, q + 2]
            : MoveAdd4 (q + 1) [r]
            : rawModulo q
            : MoveAdd4 (q + 1) [ p ]
            : Free4 (q + 2)
            : Lock4 (q + 3)
            : Lock4 (q + 4)
            : bfs (q + 3) (q + 4)
            ++ ( Free4 (q + 3)
               : Free4 (q + 4) : bf)) (s', p)
convert43' (Raw4 b p q : bf) (s, _)
  = Raw3 b p q : convert43' bf (s, q)
convert43' (Debug4 p : _) (s, _)
  = error $ show  $ Map.lookup p s
convert43' (FreeZero4 q : bf) (s, p)
  = convert43' bf (Map.delete q s, p)
convert43' b@(h : _) (s, p)
  | isFreeLike4 h
  = let (fs, rest) = span isFreeLike4 b
        frees = nubBy (\x y -> unFree4 x == unFree4 y) fs
        freepsraw = map unFree4 $ filter isFree4 frees
        freepsfilter = map unFree4 $ filter nonzero frees
        s' = foldl (flip Map.delete) s freepsraw
        in if null freepsfilter
              then convert43' rest (s', p)
              else let pmin = minimum freepsfilter
                       pmax = maximum freepsfilter
                       sorter d c
                         | abs (pmin - p) >= abs (pmax - p)
                                     = if d < c then GT else LT
                         | otherwise = if d > c then GT else LT
                       freeps = sortBy sorter freepsfilter
                       in map Free3 freeps ++ convert43' rest (s', last freeps)
    where
      isFreeLike4 (Free4 _) = True
      isFreeLike4 (AssignZero4 _) = True
      isFreeLike4 _ = False
      unFree4 (Free4 x) = x
      unFree4 (AssignZero4 x) = x
      unFree4 _ = -1
      nonzero (Free4 r) = case Map.lookup r s of
                               Just DontKnow -> True
                               Just NonZero -> True
                               Just Lock -> True
                               _ -> False
      nonzero _ = True
convert43' [] _ = []

searchNear :: Map.Map BFPointer BF4Flag -> BFPointer -> BFPointer
searchNear = searchNear' isNothing

searchNearT :: Map.Map BFPointer BF4Flag -> BFPointer -> BFPointer
searchNearT = searchNear' isNothingOrJustZero

searchNear' :: (Maybe BF4Flag -> Bool)
                      -> Map.Map BFPointer BF4Flag -> BFPointer -> BFPointer
searchNear' f st p = head [ x |
               x <- (+p) <$> iterate (\x -> if x > 0 then - x else - x + 1) 0,
                 x >= 0, f (Map.lookup x st)]

requireBlock :: Map.Map BFPointer BF4Flag -> BFPointer -> BFPointer -> BFPointer
requireBlock = requireBlock' isNothing

-- requireBlockT :: Map.Map BFPointer BF4Flag-> BFPointer -> BFPointer -> BFPointer
-- requireBlockT = requireBlock' isNothingOrJustZero

requireBlock' :: (Maybe BF4Flag -> Bool)
             -> Map.Map BFPointer BF4Flag -> BFPointer -> BFPointer -> BFPointer
requireBlock' f st p n = head [ x | x <- [p..],
                   all (f . flip Map.lookup st) [x..x+n-1] ]

isNothingOrJustZero :: Maybe BF4Flag -> Bool
isNothingOrJustZero x = isNothing x || x == Just Zero

isFree4 :: BF4 -> Bool
isFree4 (Free4 _) = True
isFree4 _ = False

