module Data where

type Id = String
type Value = Term
type Env = [(Id, Value)]
type Env1 = [(Id,Value2)]

data Term = Const Int
 | Var Id 
 | Lambda Id Term
 | App Term Term
 | Term :+ Term
 | Term :- Term
 | Term :/ Term
 | Term :* Term
 | Ifzero Term Term Term
 | Let Id Term Term
 | Fix Term
 deriving(Show)

data Value2 = Prim Int
 | Closure Term Env
 deriving(Show)
