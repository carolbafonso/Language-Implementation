module Unify where
	import Data
	import Func
	import Debug.Trace
	unify :: Type -> Type -> Maybe Subst
	unify (TCon _) (TCon _) = Just []
	unify (TVar _) (TVar _) = Just []
	unify (TVar v) t
	  | freevtype v (TP t) = Nothing
	  | otherwise = Just [(t,v)] 
	unify t (TVar v)
	  | freevtype v (TP t) = Nothing
	  | otherwise = Just [(t,v)]
	unify (TFunc t1 t2) (TFunc t3 t4) =  Just (substitutionSS s1 s2) where
		s1 = case (unify t1 t3) of
			Just a -> a
			Nothing ->error "Not unifiable, type errors"
		s2 = case (unify (substitutionST s1 t2) (substitutionST s1 t4)) of
			Just a -> a
			Nothing -> error "Not unifiable, check aplication subst-type"