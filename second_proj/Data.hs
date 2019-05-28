module Data where

--Uni
type Id = String
type TVary = String
type Subst = [(Type,TVary)]
type Env = [(Id, TScheme)]

data Expr = Const String
 | Var Id 
 | Lambda Id Expr
 | App Expr Expr
 | Let Id Expr Expr
 deriving(Show)

data TScheme = Forall [TVary] Type
 | TP Type
 deriving(Show)

data Type = TVar TVary
 | TCon String
 | TFunc Type Type
 deriving(Show)

typeInt, typeBool :: Type
typeInt = TCon "Int"
typeBool = TCon "Bool"

--Infer

type Infer = (Subst, Type)
