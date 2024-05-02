module Formalize.Syntax.Grin where

import Formalize.Scope (Atom, Binder, In, Pair, Scope, Sub)

data Name = Only
              deriving Show

data Names = NNil Atom
           | NCons (Pair Name Names)
               deriving Show

data Val = Var Name
             deriving Show

data Term = Return Val
          | AppDef In Names
          | Bind Scope (Pair Term (Binder Term))
              deriving Show

data Definition = MkDef{vars :: Scope, varsUsage :: Sub,
                        term :: Term}
                    deriving Show

