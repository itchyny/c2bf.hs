{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Brainfuck.C2BF.BF0
  ( BF0Code
  , BF0
  , parse
  , rawModulo
  , rawModulo'
  ) where
import Control.Monad.Instances ()
import Data.Functor ((<$>))
import Language.Brainfuck.C2BF.BF1

-- | Layer 0 Brainfuck
--
-- Lowest layer, which is actually String, the code of Brainfuck.
type BF0Code = String

type BF0 = Char

instance BF Char where
  convertOption _ = parse
  upconvert = show

-- | parse Brainfuck source code
parse :: BF0Code -> BFCode
parse = snd . parse'

parse' :: BF0Code -> (BF0Code, BFCode)
parse' ('+':bf) = (Incr:) <$> parse' bf
parse' ('-':bf) = (Decr:) <$> parse' bf
parse' ('>':bf) = (Next:) <$> parse' bf
parse' ('<':bf) = (Prev:) <$> parse' bf
parse' ('.':bf) = (Put:) <$> parse' bf
parse' (',':bf) = (Get:) <$> parse' bf
parse' ('[':bf) = (\(s, bf') -> (While bf':) <$> parse' s) (parse' bf)
parse' (']':bf) = (bf, [])
parse' (_:bf)   = parse' bf
parse' []       = ([], [])

-- | Raw modulo procedure
--
-- pointer p  p+1  p+2   p+3  p+4
--
-- before  n  0    d
--
-- after   0  n    d-n%d n%d  n/d
rawModulo :: BFRaw a => BFPointer -> a
rawModulo p = raw (parse "[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]") p p

-- | Raw modulo procedure
--
-- pointer p  p+1   p+2  p+3
--
-- before  n  d
--
-- after   0  d-n%d n%d  n/d
rawModulo' :: BFRaw a => BFPointer -> a
rawModulo' p = raw (parse "[->-[>+>>]>[+[-<+>]>+>>]<<<<<]") p p

