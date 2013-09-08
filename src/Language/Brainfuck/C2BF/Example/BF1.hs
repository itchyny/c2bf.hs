module Language.Brainfuck.C2BF.Example.BF1
  ( hello1
  , fib1
  ) where

import Language.Brainfuck.C2BF.BF0
import Language.Brainfuck.C2BF.BF1
import Language.Brainfuck.C2BF.Example.BF0

hello1 :: BFCode
hello1 = parse hello0

fib1 :: BFCode
fib1 = parse fib0

