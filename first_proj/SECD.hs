import Commands 
import Data
import Data.Maybe
import Data.List

compile :: Term -> Symtable -> Code
compile(Const k) sym = [LDC k]
compile(Var x) sym = [LD i] 
    where i = elemindex x sym
compile(Lambda id e1) sym = [LDF (compile e1 sym' ++ [RTN])] 
    where sym' = id:sym
compile(App e1 e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [AP]
compile(e1 :+ e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [ADD]
compile(e1 :- e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [SUB]
compile(e1 :* e2) sym = (compile e1 sym) ++ (compile e2 sym) ++ [MULT]
compile(Ifzero e0 e1 e2) sym = compile e0 sym ++ [SEL c1 c2] 
    where c1 = compile e1 sym ++ [JOIN] 
          c2 = compile e2 sym ++ [JOIN]
compile(Let id e1 e2) sym = compile (App (Lambda id e2) e1) sym
compile(Fix (Lambda f (Lambda x e))) sym = [LDRF (compile e sym' ++ [RTN])]
      where sym' = x:f:sym

{-elemindexInt :: a -> [a] -> Int
elemindexInt n l = unpack $ elemIndex n l

unpack :: Maybe Int -> Int
unpack (Maybe a) = a
unpack Nothing = -1
-}
elemindex :: Id -> Symtable -> Int
elemindex i (x:xs)
 | i == x = 0
 | otherwise = 1+(elemindex i xs)

execute :: SECD -> SECD
execute(stack, env, (LD i):c, dump, m) = (vi:stack, env, c, dump, m) where vi = (valueofIndex i env)
execute(stack, env, (LDC k):c, dump,m) = ((PrimSECD k):stack,env,c,dump,m)
execute(stack, env, (LDF c'):c, dump, m) = ((Ad a):stack, env, c, dump, (a,(c',env)):m) where a = (next m)u*
execute(stack, env, (LDRF c'):c, dump, m) = ((Ad a):stack, env, c, dump, m') where a = next m
                                                                                   m' = (a,(c',(Ad a):env)):m
execute(v:stack, env, RTN:c,(stack',env',c'):dump, m) = (v:stack',env',c',dump,m)
execute((PrimSECD 0):stack,env,(SEL c1 c2):c, dump, m) = (stack, env, c1, ([],[],c):dump, m)
execute(v:stack, env, (SEL c1 c2):c, dump, m) = (stack, env, c2, ([],[],c):dump, m)
execute(stack, env, JOIN:c, (_,_,c'):dump, m) = (stack, env, c', dump, m)
execute(v:(Ad a):stack, env, AP:c, dump, m) = ([],v:e',c',(stack,env,c):dump,m) where (c',e') = lookupClosure a m 
execute((PrimSECD v2):(PrimSECD v1):stack, env, ADD:c, dump, m) = ((PrimSECD (v1+v2)):stack, env, c, dump, m)
execute((PrimSECD v2):(PrimSECD v1):stack, env, SUB:c, dump, m) = ((PrimSECD (v1-v2)):stack, env, c, dump, m)
execute((PrimSECD v2):(PrimSECD v1):stack, env, MULT:c, dump, m) = ((PrimSECD (v1*v2)):stack, env, c, dump, m)
execute(stack,env,[HALT],dump,m) = (stack,env,[],dump,m)

valueofIndex:: Int -> EnvSECD -> ValueSECD
valueofIndex i env = env!!i

next:: Memory -> Addr
next [] = 0
next ((ad,cl):xs) = ad + 1

lookupClosure:: Addr -> Memory -> Closure
lookupClosure a [] = ([],[])
lookupClosure a ((adr,cl):xs)
 | a == adr = cl
 | otherwise = lookupClosure a xs

run :: Code -> IO()
run code = sequence_[print state | state <- trace] where
    states = iterate execute([],[],code++[HALT],[],[])
    trace = takeWhile(not.final) states
    final (stack,env,c,dump,m) = null c

