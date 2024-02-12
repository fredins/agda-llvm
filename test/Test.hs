
import Test.Tasty.QuickCheck
import Test.Tasty 
import Test.QuickCheck 

{- 
     
   Properties to test/prove: 

     - Reachability. All heap nodes are reachable from the pointers at the stack. 
       For any heap H : Loc → HeapNode and stack S : Abs → Val, the domain dom(H) ∈ image(S).

     - Inverse Reachability. All pointers points to a location that is part of the heap. 
       Thereby, we have no "use after free".
       
     - No garbage. Given reachability, we can prove that there is no garbage left behind, i.e. 
       all the memory is deallocated when the program finishes (empty heap). 
       TODO: make local, doesn't touch the initial heap.

     - Number of reference counts equal to number of actual references. 
       TODO: define "actual references"

     - Precision. Nodes are deallocated as soon as the node is no longer need. 
       (Reiking et al. proves this by proving soundness from the syntax-directed 
       linear rules to declaraitve linear syntax rules.)
-}


main = putStrLn "Hello, World!"
