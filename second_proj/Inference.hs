import Data
import Unify
import Func 

infer :: Env -> Expr -> Type -> Infer
infer env (Const c) _
 | isBool c = ([], typeBool)
 | otherwise = ([], typeInt)
infer env (Var v) _ = lookupEnv env v
infer env (Lambda id e) fvar = (s,t) where
	nv = fresh fvar
	(s1, t1) = infer ((id,(TP nv)):env) e nv
	s = s1
	t = TFunc (substitutionST s1 nv) (t1)
infer env  (Let id e1 e2) fvar = (s,t) where
	(s1, t1) = infer env e1 fvar
	(s2, t2) = infer ((id,t'):env') e2 fvar where
		env' = applyaux s1 env
		t' = generalize env' t1
	s = substitutionSS s1 s2
	t = t2
infer env (App e1 e2) fvar = (s,t) where
    nv = fresh fvar
    fnv = fresh nv
    ffnv = fresh fnv
    (s1, t1) = infer env e1 fnv
    (s2, t2) = infer(applyaux s1 env) e2 ffnv
    s3 = unify (substitutionST s1 t1)(TFunc t2 nv)
    s4 = unpack s3
    s = (substitutionSS (substitutionSS s4 s2) s1)
    t = substitutionST s4 nv
 