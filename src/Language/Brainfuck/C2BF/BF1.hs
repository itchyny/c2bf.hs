module Language.Brainfuck.C2BF.BF1
  ( BF (..)
  , BFCode
  , BFPrim (..)
  , BFRich (..)
  , BFRaw (..)
  , BFPointer
  , BFInt
  , Option (..)
  , o0, o1, o2, o3, defaultOption
  , convert11Option
  ) where
import Codec.Binary.UTF8.String (encode, decode)
import Control.Monad.Instances
import Data.Char (chr)
import Data.Functor ((<$>))
import Data.Word (Word8)
import Data.List (delete, nub)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, hFlush, stdout, stderr)
import System.Process (readProcessWithExitCode)
import Language.Brainfuck.C2BF.Util

-- | Brainfuck class
class BF a where
  -- | All Brainfuck like things can be converted to `BFCode'
  convert :: [a] -> BFCode
  convert = convertOption defaultOption
  -- | You can specify optimization options.
  -- `convert' = `convertOption' `defaultOption'
  convertOption :: [Option] -> [a] -> BFCode
  convertOption = undefined
  -- | Reverse of convert. You can convert Brainfuck code to higher layer.
  upconvert :: BFCode -> [a]
  upconvert = undefined
  -- | Convert and print the Brainfuck code
  printBF :: [a] -> IO ()
  printBF = putStrLn . showBF
  -- | Convert and show the Brainfuck code
  showBF :: [a] -> String
  showBF = show . convert
  -- | Convert and run the code
  run :: [a] -> IO ()
  run = run . convert
  -- | Convert and run the code, returns the output string
  runStr :: [a] -> IO String
  runStr = runStr . convert
  -- | Convert and run the code strictly, returns the output string.
  -- You cannot use with codes including an input instruction (,)
  runStrStrict :: [a] -> String
  runStrStrict = runStrStrict . convert
  -- | Convert and run the code using external program bf
  runExt :: [a] -> IO ()
  runExt = runExt . convert
  -- | Convert and run the code using external program bf, returns the output
  runExtStr :: [a] -> IO String
  runExtStr = runExtStr . convert

infixl 5 +=, -=, *=, //=
class BFRich a where
  put, get :: BFPointer -> a
  (+=), (-=), (*=), (//=) :: BFPointer -> BFInt -> a
  (+=) = undefined
  (-=) = undefined
  (*=) = undefined
  (//=) = undefined

class BFRaw a where
  raw :: BFCode -> BFPointer -> BFPointer -> a

-- | Layer 1 Brainfuck
--
-- Any BF class code can be converted to this data structure.
-- Invalid codes of unmatched parenthesisare excluded at this layer.
type BFCode = [BFPrim]
data BFPrim = Incr
            | Decr
            | Next
            | Prev
            | Put
            | Get
            | While BFCode
            deriving Eq

-- | Brainfuck pointer address
type BFPointer = Integer

-- | Brainfuck pointer state (left, current, right, output stream, input stream)
type State = ([BFInt], BFInt, [BFInt], [Word8], [Word8])

-- | Brainfuck pointer number
type BFInt = Int

-- | Optimization option
data Option
  = TailingFree     -- ^ Remove last variable free:  [-]<[-]<[-] (at code ends)
  | LazyInitialization -- ^ Make initialization lazy at the head of the code
  | WhileSuccession -- ^ Remove while succession: [ foo ] [ unreachable ]
  | HeadingWhile    -- ^ Remove heading loops assuming memory cells are all zero
  | InitializeNear -- ^ Improve the heading initialization
  | TailingNonIO    -- ^ Remove non-IO instructions from the end of the code
  | OffsetAddress  -- ^ Remove unnecessary address offset
  | AddLargeNumber -- ^ (dangerous) Use free cell to add a large number at BF1
  | DropInnerFree  -- ^ (dangerous) analyze inside whiles and remove variable
                   -- frees after that loop (it should executed at least once)
  | MoveOnNonZero  -- ^ (not implemented) analyze non zero cells and move by [>]
  deriving Eq

-- | Optimization option containers
--
-- o0 : no optimization
--
-- o1 : WhileSuccession, TailingFree, LazyInitialization
--
-- o2 : HeadingWhile, InitializeNear
--
-- o3 : OffsetAddress, AddLargeNumber, TailingNonIO, DropInnerFree
o0, o1, o2, o3 :: [Option]
o0 = []
o1 = WhileSuccession : TailingFree : LazyInitialization : o0
o2 = HeadingWhile : InitializeNear : o1
o3 = OffsetAddress : AddLargeNumber : TailingNonIO : DropInnerFree : o2

-- | Default optimization option: o1
defaultOption :: [Option]
defaultOption = o1

instance Show BFPrim where
  show Incr       = "+"
  show Decr       = "-"
  show Next       = ">"
  show Prev       = "<"
  show Put        = "."
  show Get        = ","
  show (While bf) = "[" ++ concatMap show bf ++ "]"
  showList = (++) . concatMap show

instance BF BFPrim where
  convertOption = convert11Option
  upconvert = id
  run = runBFCode
  runStr = runBFCodeStr
  runStrStrict = runBFCodeStrict
  runExt bf = runExtWithExitCode bf >>= \(_, o, e) -> putStrLn o
                                                   >> hFlush stdout
                                                   >> hPutStrLn stderr e
                                                   >> hFlush stderr
  runExtStr = runExtBFCode

runBFCode :: BFCode -> IO ()
runBFCode bf = run' True bf ([], 0, [], [], [])
                       >>= \(_, _, _, o, _) -> putStrLn (decode o)

runBFCodeStr :: BFCode -> IO String
runBFCodeStr bf = run' False bf ([], 0, [], [], [])
                           >>= \(_, _, _, o, _) -> return (decode o)

runBFCodeStrict :: BFCode -> String
runBFCodeStrict bf = case runS bf ([], 0, [], [], []) of
                          Left (_, _, _, x, _) -> decode x
                          Right s -> "error: " ++ s

run' :: Bool -> BFCode -> State -> IO State
run' b (Incr:bs) (xs, x, ys, o, i)   = run' b bs (xs, mod (x + 1) 256, ys, o, i)
run' b (Decr:bs) (xs, x, ys, o, i)   = run' b bs (xs, mod (x - 1) 256, ys, o, i)
run' b (Next:bs) (xs, x, [], o, i)   = run' b bs (x:xs, 0, [], o, i)
run' b (Next:bs) (xs, x, y:ys, o, i) = run' b bs (x:xs, y, ys, o, i)
run' _ (Prev:_) ([] , _, _, _, _)    = error "negative address access!"
run' b (Prev:bs) (x:xs, y, ys, o, i) = run' b bs (xs, x, y:ys, o, i)
run' b (While _:bs) s@(_, 0, _, _, _) = run' b bs s
run' b bbs@(While bs:_) s            = run' b bs s >>= run' b bbs
run' b (Put:bs) (xs, x, ys, o, i) -- assuming UTF-8
  | b && (x >= 256) = putStr str >> run' b bs (xs, x, ys, [], i)
  | b && (x >= 192) = putStr (decode o) >> run' b bs (xs, x, ys, [toEnum x], i)
  | b && (null o || x == 10) = putStr str >> run' b bs (xs, x, ys, [], i)
  | otherwise = run' b bs (xs, x, ys, o ++ [toEnum x], i)
  where str = decode o ++ [chr x]
run' b (Get:bs) (xs, _, ys, o, [])   = encode . return <$> getChar >>=
                            \(i:is) -> run' b bs (xs, fromEnum i, ys, o, is)
run' b (Get:bs) (xs, _, ys, o, i:is) = run' b bs (xs, fromEnum i, ys, o, is)
run' _ [] s = return s

runS :: BFCode -> State -> Either State String
runS (Incr:bs) (xs, x, ys, o, i)   = runS bs (xs, mod (x + 1) 256, ys, o, i)
runS (Decr:bs) (xs, x, ys, o, i)   = runS bs (xs, mod (x - 1) 256, ys, o, i)
runS (Next:bs) (xs, x, [], o, i)   = runS bs (x:xs, 0, [], o, i)
runS (Next:bs) (xs, x, y:ys, o, i) = runS bs (x:xs, y, ys, o, i)
runS (Prev:_) ([] , _, _, _, _)    = Right "negative address access!"
runS (Prev:bs) (x:xs, y, ys, o, i) = runS bs (xs, x, y:ys, o, i)
runS (While _:bs) s@(_, 0, _, _, _) = runS bs s
runS bbs@(While bs:_) s            = case runS bs s of
                                                Left a -> runS bbs a
                                                Right b -> Right b
runS (Put:bs) (xs, x, ys, o, i) = runS bs (xs, x, ys, o ++ [toEnum x], i)
runS (Get:_) (_, _, _, _, _)    = Right "cannot use Get in runStrStrict"
runS [] s = Left s

-- I recommend using my program:
--     curl https://raw.github.com/itchyny/brainfuck/master/bf.c > bf.c \
--       && gcc -O3 ./bf.c -o bf && sudo cp ./bf /usr/bin/
-- The program bff (https://github.com/apankrat/bff, ideone.com) is inefficient
-- because it uses int instead of char for memory cells.
runner :: String
runner = "bf"

runExtBFCode :: [BFPrim] -> IO String
runExtBFCode x = runExtWithExitCode x >>= \(_, o, _) -> return o

runExtWithExitCode :: [BFPrim] -> IO (ExitCode, String, String)
runExtWithExitCode x = readProcessWithExitCode runner [show x] []

convert11Option :: [Option] -> BFCode -> BFCode
convert11Option opt
  | elem WhileSuccession opt
    = convert11Option (delete WhileSuccession opt) . removeWhileSucc
  | elem TailingNonIO opt
    = convert11Option (delete TailingNonIO opt) . removeTailingNonIO
  | elem AddLargeNumber opt
    = convert11Option (delete AddLargeNumber opt) . analizeLargeNumber
  | otherwise = id

removeWhileSucc :: BFCode -> BFCode
removeWhileSucc (While bfs : bf)
  = While (removeWhileSucc bfs) : removeWhileSucc (dropWhile isWhile bf)
  where
    isWhile (While _) = True
    isWhile _ = False
removeWhileSucc (b : bf) = b : removeWhileSucc bf
removeWhileSucc [] = []

removeTailingNonIO :: BFCode -> BFCode
removeTailingNonIO = dropLastWhile nonIO
  where
    nonIO (While bfs) = all nonIO bfs
    nonIO Put = False
    nonIO Get = False
    nonIO _ = True

analizeLargeNumber :: BFCode -> BFCode
analizeLargeNumber = f [0..50]
  where
    f :: [Int] -> BFCode -> BFCode
    f el (While bfs:bf) = While (f [] bfs) : f (nub (0:el)) bf -- TODO:dangerous
    f el (Next:bf) = Next : f (map (subtract 1) el) bf
    f el (Prev:bf) = Prev : f (map (+1) el) bf
    f el (Put:bf) = Put : f el bf
    f [] (Incr:bf) = Incr : f [] bf
    f [0] (Incr:bf) = Incr : f [] bf
    f el (bf@(Incr:_))
      = let (incrs, rest) = span isIncr bf
            len = length incrs
            d = snd $ minimum $ map (\x -> (abs x, x)) (delete 0 el)
            in if length incrs < 16 || abs d > 9
                  then incrs ++ f (delete 0 el) rest
                  else let part = partconst len
                           m = replMove d
                           m' = replMove (-d)
                           in case part of
                                   Left _ -> incrs ++ f (delete 0 el) rest
                                   Right (a, b, c) ->
                                     let as = replicate a Incr
                                         bs = replicate b Incr
                                         cs | c < 0 = replicate (-c) Decr
                                            | otherwise = replicate c Incr
                                         in (m ++ as
                                               ++ [While (Decr : m' ++ bs ++ m)]
                                               ++ m'
                                               ++ cs
                                               ++ f (delete 0 el) rest)
    f el (b:bf) = b : f (delete 0 el) bf
    f _ bf = bf
    isIncr Incr = True
    isIncr _ = False
    replMove d | d < 0 = replicate (-d) Prev
               | otherwise = replicate d Next

