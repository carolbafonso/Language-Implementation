module Func where
	import Data

    --Unify
    -- Substitution (subst/subst)
	substitutionSS :: Subst -> Subst -> Subst
	substitutionSS s [] = s
	substitutionSS [] s = s
	substitutionSS ((t,v):s1) s2 = (applysubst (t,v) s2) ++ (substitutionSS s1 s2)

	applysubst :: (Type, TVary) -> Subst -> Subst
	applysubst _ [] = []
	applysubst (t,v) ((t1,v1):xs)
	  | v == v1 = ((t,v1):(applysubst (t,v) xs))
	  | otherwise = applysubst (t,v) xs
    -- end of substitution

    ---- Substitution (subst/type)
	substitutionST :: Subst -> Type -> Type
	substitutionST [] t = t
	substitutionST ((t1,v):s) t = substitutionST s (applyType (t1,v) t)

	applyType :: (Type, TVary) -> Type -> Type
	applyType (t,v) (TCon p) = (TCon p)
	applyType (t,v) (TVar v1)
	  | v == v1 = t
	  | otherwise = TVar v1
	applyType (t,v) (TFunc t1 t2) = (TFunc (applyType (t,v) t1) (applyType (t,v) t2))
	-- end of substitution

	-- check if the variable is free in the TScheme
	freevtype :: TVary -> TScheme -> Bool
	freevtype v t
	  | v `elem` (varfreeTS t) = True
	  | otherwise = False

	-- all free variables in the TScheme
	varfreeTS :: TScheme -> [TVary]
	varfreeTS (Forall l t) = (rmvar l (freevarT t))
	varfreeTS (TP t) = freevarT t

	-- all free variables in the Type
	freevarT :: Type -> [TVary]
	freevarT (TVar t) = [t]
	freevarT (TCon t) = []
	freevarT (TFunc t1 t2) = (freevarT t1) ++ (freevarT t2)

   -- remove all bounded variable from the list of variables
	rmvar :: [TVary] -> [TVary] -> [TVary]
	rmvar (x:xs) l2 = (findelemlist x l2) ++ (rmvar xs l2)

	findelemlist :: TVary -> [TVary] -> [TVary]
	findelemlist v (x:xs)
	  | v == x = (findelemlist v xs)
	  | otherwise = (x:(findelemlist v xs))
    -- end of remove 

	-- infer
	-- Free variables in Env
	varfreeEnv :: Env -> [TVary]
	varfreeEnv [] = []
	varfreeEnv ((id,ts):xs) = (varfreeTS ts) ++ (varfreeEnv xs)

   -- remove maybe from the type subst
	unpack :: Maybe Subst -> Subst
	unpack (Just a) = a
	unpack Nothing = error "Nothing was returned"
    
    -- find a variable in env
	lookupEnv :: Env -> Id -> Infer
	lookupEnv [] _ = error "Env is null"
	lookupEnv ((id,(TP ts)):xs) v
	  | id == v = ([], ts)
	  | otherwise = lookupEnv xs v
	lookupEnv ((id, (Forall l t)):xs) v
	  | id == v = ([], t)
	  | otherwise = lookupEnv xs v

	isBool :: String -> Bool
	isBool "True" = True
	isBool "False" = True
	isBool _ = False
    
    -- generate a new variable
	fresh :: Type -> Type
	fresh (TVar v) = (TVar ("|" ++ v ++ "|"))

    -- substitution (subst/env)
	applyaux :: Subst -> Env -> Env
	applyaux _ [] = []
	applyaux [] e = e
	applyaux ((t,v):xs) e = (applySEnv (t,v) e) ++ (applyaux xs e)

	applySEnv :: (Type, TVary) -> Env -> Env
	applySEnv _ [] = []
	applySEnv (t,v) ((id,ts):xs)
	  | v == id = (id, (TP t)):(applySEnv (t,v) xs)
	  | otherwise = (id,ts):(applySEnv (t,v) xs)
    -- end of substitution
    
    -- remove all variables from Type in the env
	generalize :: Env -> Type -> TScheme
	generalize e t = (Forall as t) where
		as = rmvar (freevarT t) (varfreeEnv e)