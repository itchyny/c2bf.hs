module Language.Brainfuck.C2BF.Example.BF3
  ( hello3
  , fib3
  ) where

import Data.Char (ord)
import Language.Brainfuck.C2BF.BF3

hello3 :: BF3Code
hello3 = [ AddI3 0 9
         , Mult3 0 [(8, 1), (11, 2), (5, 3)], Put3 1
         , AddI3 2 2 , Put3 2, AddI3 2 7, Put3 2, Put3 2
         , AddI3 2 3 , Put3 2, SubI3 3 1, Put3 3
         , SubI3 3 12, Put3 3, AddI3 2 8, Put3 2
         , SubI3 2 8 , Put3 2, AddI3 2 3, Put3 2
         , SubI3 2 6 , Put3 2, SubI3 2 8, Put3 2
         , AddI3 3 1 , Put3 3
         ]

-- taken from http://esoteric.sange.fi/brainfuck/bf-source/prog/fibonacci.txt
fib3 :: BF3Code
fib3 = [ AddI3 0 11
       , AddI3 1 1
       , AddI3 5 (ord ',')
       , AddI3 6 (ord ' ')
       , While3 0
         [ Mult3 1 [ (1, 7), (1, 8) ], Mult3 8 [ (1, 1) ]
         , While3 7
           [ AddI3 8 10
           , While3 8
             [ SubI3 8 1
             , SubI3 7 1
             , Mult3 7 [ (1, 9), (1, 10) ], Mult3 10 [ (1, 7) ]
             , AddI3 10 1
             , If3 9 [ Free3 10 ]
             , If3 10 [ Mult3 8 [ (1, 11) ] ]
             ]
           , Mult3 11 [ (1, 13), (1, 14) ], Mult3 14 [ (1, 11) ]
           , AddI3 14 1
           , If3 13 [ Free3 14 ]
           , If3 14 [ AddI3 12 1 ]
           ]
           , If3 12 [ AddI3 12 48, Put3 12 ]
           , AddI3 12 10
           , Mult3 11 [ (-1, 12) ]
           , AddI3 12 48
           , Put3 12
           , Free3 12
           , Mult3 0 [ (1, 3), (1, 4) ], Mult3 4 [ (1, 0) ]
           , SubI3 3 1
           , If3 3 [ Put3 5, Put3 6 ]
           , Mult3 1 [ (1, 3), (1, 4) ], Mult3 4 [ (1, 1) ]
           , Mult3 2 [ (1, 1) ], Mult3 3 [ (1, 2) ]
           , SubI3 0 1
         ]
       ]

