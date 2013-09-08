-- sudo cabal install language-c utf8-string containers
module Main where
import Control.Monad.Instances
import Data.List (unfoldr)
import Data.Maybe (fromJust, isNothing)
import Language.Brainfuck.C2BF
import Language.Brainfuck.C2BF.Example
import Language.C
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Data.InputStream
import Language.C.Syntax.AST
import Language.C.System.GCC
-- haddock -h -o html/ c2bf.hs && open ./html/Language-Brainfuck-C2BF.html

main :: IO ()
main = do

  let code = pic4

  -- a <- readFile "../brainfuck/hs2bf_hello.bf"
  -- let res = parse a
  -- let code = (upconvert $ parse a) :: BF3Code
  -- let code = (upconvert $ While [Decr] : Next : While [Decr] : Next : While [Decr] : Prev : Prev : replicate 72 Incr ++ [Put]) :: BF3Code

  -- let code = pic4
  -- let code = (upconvert $ parse ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.") :: BF3Code
  -- print code
  let code = fib5
  -- let code = [ InitChar5 30 (\x -> [ While5 x [ InitLong5 999999 (\y -> [ PrintD5 y ,  PrintD5 y , PrintD5 y ]), SubI5 x 1 ] ]) ]
  let code0 = convertOption o0 code
  let code1 = convertOption o1 code
  let code2 = convertOption o2 code
  let code3 = convertOption o3 code

  -- print $ length $ showBF $ code
  -- print $ length $ showBF $ code0
  -- print $ length $ showBF $ code1
  -- print $ length $ showBF $ code2
  -- print $ length $ showBF $ code3
  mapM_ putStrLn $ split80 $ showBF $ code3
  -- mapM_ putStrLn $ split80 $ showBF $ code0

  -- printBF $ convert code
  -- printBF $ convertOption o0 code
  -- printBF $ convertOption o1 code
  -- printBF $ convertOption o2 code
  -- printBF $ convertOption o3 code

  -- runExt $ convert code
  -- runExt $ convertOption o0 code
  -- runExt $ convertOption o1 code
  -- runExt $ convertOption o2 code
  runExt $ convertOption o3 code




  -- print hello0
  -- print $ ((fib0))
  -- print $ convert (upconvert (parse fib0) :: BF3Code)
  -- -- print hello1
  -- -- print hello2
  -- print hello3
  -- print hello4

  -- print fib0
  -- print fib1
  -- print fib2
  -- print fib3
  -- print fib4
  -- print prime4
  -- print pic4

  -- printBF fib4 -- 339
  -- putStrLn $ runStrStrict fib4
  -- printBF countdown4 --261
  -- runExt countdown4
  -- printBF prime4 -- 718
  -- runExt prime4
  -- printBF pic4 -- 774
  -- runExt pic4
  -- printBF pic_4 -- 1911
  -- runExt pic_4
  -- printBF pic'4 -- 1915
  -- runExt pic'4

  -- let fib5' = convertOption o3 fib5
  -- printBF fib5' -- 378 (counter=13), 1890 (counter=24), 5120 (counter=47)
  -- runExt fib5'
  -- printBF twoInt5 -- 1892, 5075
  -- runExt twoInt5
  -- printBF countupInt5 -- 4994
  -- runExt countupInt5
  -- printBF echo5 -- 7
  -- printBF hello5 -- 116
  -- -- putStrLn $ runStrStrict hello5
  -- let a = [PrintS5 "∩(＞◡＜*)∩♡o-(-ω<｡*✿O"]  -- 382
  -- printBF a
  -- run a
  -- let a = [PrintS5 "o-(-ω<｡*✿O+。:.ﾟ٩(๑>◡<๑)۶:.｡+ﾟ❀ c(╹◡╹❀) o-(-ω<｡*✿O"]  -- 867
  -- printBF a
  -- run a

  -- flip mapM_ [0..div 255 2] $ \a -> do
  -- flip mapM_ [255,255+256..(255 + 255 * 256)] $ \a -> do
  --   let b = [ InitLong5 a $ \x->
  --           [ InitLong5 0 $ \y->
  --           [ Assign5 y x
  --           , MoveAdd5 y x
  --           , PrintD5 x
  --           ] ] ]
  --   r <- runExtStr b
  --   if r /= show (2 * a)
  --      then putStrLn $ "error: " ++ r ++ " /= " ++ show (2 * a)
  --      else putStrLn $ "ok:    " ++ r ++ " == " ++ show (2 * a)

  -- a <- readFile "../brainfuck/hs2bf_hello.bf"
  -- let res = parse a
  -- let b = (upconvert $ parse a) :: BF3Code
  -- let c = convertOption O3 b
  -- print $ length $ showBF $ res
  -- print $ length $ showBF $ b
  -- print $ length $ showBF $ c
  -- 124711
  -- 121834

  -- let filename = "test.c"
  -- stream <- readInputStream filename
  -- let result = parseC stream (initPos filename)
  -- case result of
  --      Right (CTranslUnit xs _) ->
  --        let (CFDefExt (CFunDef _ _ _ (CCompound _ ma _) _)) = head xs
  --            code = convertOption o0  (convert65 ma [])
  --            in do mapM_ print ma
  --                  printBF code
  --                  runExt code
  --      Left a -> print a

split80 :: [a] -> [[a]]
split80 = unfoldr $ \x -> if null x then Nothing else Just $ splitAt 80 x

convert65 (CBlockDecl (CDecl typedecl ((Just body, _, _):rest) a):xs) vars
  = let t = (\(CTypeSpec x) -> x) $ head typedecl
        varname = (\(CDeclr (Just x) _ _ _ _) -> x) body
        initvar = case t of
                       CCharType _ -> InitChar5
                       CShortType _ -> InitShort5
                       CIntType _ -> InitLong5
                       CLongType _ -> InitLong5
        xxs = CBlockDecl (CDecl typedecl rest a) : xs
        in [initvar 0 (\v -> convert65 xxs ((varname, v) : vars))]
convert65 (CBlockStmt (CExpr (Just (CAssign CAssignOp x y _)) _):xs) vars
  = let varname = (\(CVar x _) -> x) x
        a = (\(CVar x a) -> a) x
        val = fromInteger $ (\(CConst (CIntConst (CInteger n _ _) _)) -> n) y
        var = getvar varname vars
        in if isNothing var
              then errorVarNotFound varname a
              else AddI5 (fromJust var) val : convert65 xs vars
convert65 (CBlockStmt (CExpr (Just (CCall p@(CVar (Ident ('p':'r':'i':'n':'t':'f':[]) _ _) _) (CConst (CStrConst (CString ('%':'d':cs) a3) a4) : CVar varname a : rest) a5)) a6):xs) vars
  = let var = getvar varname vars
        xss = CBlockStmt (CExpr (Just (CCall p (CConst (CStrConst (CString cs a3) a4) : rest) a5)) a6) : xs
        in if isNothing var
              then errorVarNotFound varname a
              else PrintD5 (fromJust var) : convert65 xss vars
convert65 (CBlockStmt (CExpr (Just (CCall p@(CVar (Ident ('p':'r':'i':'n':'t':'f':[]) _ _) _) (CConst (CStrConst (CString (c:cs) a3) a4) : rest) a5)) a6):xs) vars
  = let xss = CBlockStmt (CExpr (Just (CCall p (CConst (CStrConst (CString cs a3) a4) : rest) a5)) a6) : xs
        in PrintS5 [c] : convert65 xss vars
convert65 (_:xs) vars
  = convert65 xs vars
convert65 [] _ = []

fstsearch :: Eq a => a -> [(a, t)] -> [(a, t)]
fstsearch b = filter (\(a, _) -> a == b)

getvar :: Eq a => a -> [(a, b)] -> Maybe b
getvar a b = let var = fstsearch a b
                 in if null var then Nothing else Just (snd (head var))

errorVarNotFound :: Ident -> NodeInfo -> a
errorVarNotFound var (NodeInfo pos _ _)
  = let fname = posFile pos
        row = posRow pos
        varname = identToString var
        in error $ fname ++ ":"
                ++ show row ++ ": error: "
                ++ wrapvar varname
                ++ " undeclared"

wrapvar :: String -> String
wrapvar var = "`" ++ var ++ "'"

-- http://hackage.haskell.org/packages/archive/language-c/0.4.2/doc/html/Language-C-Syntax-AST.html

