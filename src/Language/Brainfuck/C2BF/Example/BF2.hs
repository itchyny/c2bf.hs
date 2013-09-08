module Language.Brainfuck.C2BF.Example.BF2
  ( hello2
  , fib2
  ) where

import Language.Brainfuck.C2BF.BF2
import Language.Brainfuck.C2BF.Example.BF1

hello2 :: BF2Code
hello2 = convert12 hello1

fib2 :: BF2Code
fib2 = convert12 fib1

