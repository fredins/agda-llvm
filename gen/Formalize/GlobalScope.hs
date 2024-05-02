module Formalize.GlobalScope where

import Formalize.Scope (Scope)

data Globals = Globals{varScope :: Scope, defScope :: Scope}

