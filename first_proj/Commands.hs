module Commands where

type VarSECD = String
type Symtable = [VarSECD]
type Code = [Instr]
type EnvSECD = [ValueSECD]
type EnvSECDN = [Term]
type Stack = [ValueSECD]
type StackN = [Term]
type Dump = [(Stack, EnvSECD, Code)]
type DumpN = [(StackN,EnvSECDN,Code)]
type SECD = (Stack, EnvSECD, Code, Dump, Memory)
type SECDN = (StackN,EnvSECDN,Code,DumpN,Memory)
type Memory = [(Addr, Closure)]
type Addr = Int
type Closure = (Code, EnvSECD)

data Instr = LDC Int
 | LD Int
 | LDF [Instr]
 | LDRF [Instr]
 | AP 
 | RTN
 | SEL [Instr] [Instr]
 | JOIN
 | ADD
 | SUB
 | MULT
 | HALT
 deriving(Show)

data ValueSECD = PrimSECD Int
 | Ad Addr
 deriving(Show)

data Term = V ValueSECD
 | C Code
 deriving(Show)