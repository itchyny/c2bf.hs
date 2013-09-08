module Language.Brainfuck.C2BF.Example.BF5
  ( hello5
  , fib5
  , echo5
  , countupInt5
  , twoInt5
  ) where

import Data.Char (ord)
import Language.Brainfuck.C2BF.BF5

ord' :: (Enum a) => Char -> a
ord' = toEnum . ord

hello5 :: BF5Code
hello5 = [ PrintS5 "Hello, world" ]
-- hello5 = [ PrintS5 "ヾ(╹◡╹❀)ﾉﾞ" ]
-- hello5 = [ PrintS5 "(๑`･ᴗ･´๑)"  ]
-- hello5 = [ PrintS5 "♪♡ヾ(❀╹◡╹)ﾉﾞ❀ヾ(╹◡╹๑)ﾉ♡♪" ]
-- hello5 = [ PrintS5 "こんにちは、世界。" ]
-- hello5 = [ PrintS5 "ありがと" ]
{-
>>>+++++++++[-<<<++++++++>+++++++++++>+++++>]<-<++<.>.+++++++..+++.>.-----------
-.<++++++++.--------.+++.------.--------.>+.
+++++++++[->++++++++>+++++++++++>+++++<<<]>>++>-<<.>.+++++++..+++.>.------------
.<++++++++.--------.+++.------.--------.>+.
-}

fib5 :: BF5Code
fib5 = [ InitChar5 (ord' ',') $ \comma ->
       [ InitChar5 (ord' ' ') $ \space ->
       [ Init5 0 $ \fibb ->         -- fibb (previous value)
       [ InitChar5 17 $ \counter ->     -- counter
       [ Init5 1 $ \fiba ->         -- fiba (new value)
       [ Init5 0 $ \temp ->         -- fiba (new value)
         [ While5 counter
           [ PrintD5 fiba
           , IfPos5 counter [ Put5 comma, Put5 space ]
           , Assign5 temp fiba       -- temp = fiba
           , MoveAdd5 fibb fiba      -- fiba += fibb; fibb = 0;
           , MoveAdd5 temp fibb      -- fibb += temp; temp = 0;
           , SubI5 counter 1
       ] ] ] ] ] ] ] ]

echo5 :: BF5Code
echo5 = [ Init5 1 $ \x ->
        [ InitShort5 0 $ \c ->
          [ While5 x [ Get5 c, Put5 c ] ] ] ]

countupInt5  :: BF5Code
countupInt5 = [ Init5 (ord' ' ') $ \sp ->
              [ Init5 (ord' '\n') $ \nl ->
              [ Init5 (ord' '9') $ \nine ->
              [ Init5 0 $ \n ->
              [ Init5 101 $ \num ->
              [ InitLong5 999 $ \dx ->
              [ InitLong5 0 $ \x ->
              [ InitLong5 0 $ \d ->
                [ While5 num
                  [ Put5 nine, Put5 nine, Put5 nine
                  , Put5 sp, AddI5 sp (ord' '*' - ord' ' ')
                  , Put5 sp, SubI5 sp (ord' '*' - ord' ' ')
                  , Put5 sp
                  , PrintD5 n
                  , Put5 sp, AddI5 nine (ord' '=' - ord' '9')
                  , Put5 nine, SubI5 nine (ord' '=' - ord' '9')
                  , Put5 sp
                  , PrintD5 x, Put5 nl
                  , Assign5 d dx
                  , MoveAdd5 d x
                  , SubI5 num 1
                  , AddI5 n 1
                  ]
            ] ] ] ] ] ] ] ] ]

twoInt5 :: BF5Code
twoInt5 = [ Init5 (ord' ',') $ \comma ->
          [ Init5 (ord' ' ') $ \space ->
          [ Init5 33 $ \counter ->
          [ InitLong5 0 $ \fibb ->
          [ InitLong5 1 $ \fiba ->
          [ InitLong5 0 $ \temp ->
            [ While5 counter
              [ PrintD5 fiba
              , IfPos5 counter [ Put5 comma, Put5 space ]
              , Assign5 temp fiba
              , MoveAdd5 temp fibb
              , MoveAdd5 fibb fiba
              , SubI5 counter 1
        ] ] ] ] ] ] ] ]

heart5 :: BF5Code
heart5 = undefined
-- http://www.mathematische-basteleien.de/heart.htm


