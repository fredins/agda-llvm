
module PrimForce where

open import Agda.Builtin.Nat using (suc; zero; _-_) renaming (Nat to ℕ) 
open import Agda.Builtin.Strict 

main = primForce 10 _-_ 5

-- _ : primForce 10 _-_ 5 ≡ 5
-- _ = refl 
--
-- open import Agda.Builtin.IO using (IO)
-- open import Agda.Builtin.Unit using (⊤)
-- open import Agda.Builtin.String using (String)
-- open import Data.Nat.Show using (show)
-- open import Function using (_$_)
--
-- postulate putStrLn : String → IO ⊤
-- {-# FOREIGN GHC import qualified Data.Text as T #-}
-- {-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}
--
--
-- main : IO ⊤
-- main = putStrLn $ show $ primForce 10 _-_ 5
