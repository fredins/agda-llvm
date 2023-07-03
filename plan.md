Ideas:
• Functional but in place (FBIP)
• Strictness analysis enables passing unboxed values [1, 5].
• Deforestation and specialiation via supercompilation [2].
• Join points extension to ANF [3]?
• Destination-Passing style [4]?
• Tail recursion?
• Unsure what evaluation model i should use; either cell model 
  or self-updating model [5]
• Don't allocate when forcing x in:

    case x of 
      nil → ...
      cons y ys → ...

  only put the arguments of, for instance, `cons` in the registers [5].


Plan: 
• Use heap-allocated closures/thunks.
• Use llvm's `tailcc` to limit the stack.
• Use reference counting to allow reuse.


[1] Unboxed values as first class citizens in a non-strict functional language
[2] Supercompilation by Evaluation
[3] Compiling without Continuations
[4] Destination-Passing Style for Efficient Memory Management
[5] Implementing lazy functional languages on stock hardware: 
    the Spineless Tagless G-machine



     main = 
       let xs = 1 :: 2 :: [] in
       map (_+_ 10) xs

     map f xs = case xs of
       []      → Nil
       x :: xs → f x :: map f xs

    %info = 
      { i1   ; evaluated?
      , i8   ; reference count
      , i8   ; next_position
      , i32  ; size
      , ptr  ; function
      }

    %add_thunk = { ptr, ptr, ptr }
    
    define i32 @add(ptr %cxt){
      %x = extractvalue i32 %vars, 0
      %x = extractvalue i32 %vars, 1
      %1 = add i32 %x, %y
      ret %1
    }

    add x y = ...

    apply f x = f x

    main = apply (add 4) 5


    define i32 @add(i32 %x, i32 %y){
      %1 = add i32 %x, %y
      ret %1
    }

    define i32 @apply(ptr %f, ptr %x){
      force

    }

    define i32 @main(){

    }
    







    go : { B C : Set}. ({A : Set} → A → A) → B → C -> B 
    go f b c = const (f b) c


    main = go (λ x → add x 4) 2 (repeatelymul 4)



    define void go(ptr %f, ptr %b, ptr %c){
  

    }


    %f_thunk_type = { ptr, i32* }
    %b_thunk_type = { ptr, i32* }

    define i32 repeatelymultiply(i32 %w){
      %1 = mul i32 %w %w
      %2 = call i32 @repeatelymul(%w)
      ret %2
    }
    
    define i32 add(i32 %y, %z){
      %1 = add i32 %y %z
      ret %1
    }
    
    define i32 go(ptr %f,ptr %b,ptr %c){
      %f_deref = load ptr %f
      %b_deref = load ptr %f
      %1 = call i32 %f_deref(%b_deref)
      ret %1
    }

    define void main(){
      %f_thunk = alloca %f_thunk_type
      store ptr %sc_0, ptr %f_thunk
      store ptr %sc_0, ptr %f_thunk
      
      %2_thunk = alloca %2_thunk_type

      %b_thunk = alloca %b_thunk_type
      store ptr %repeatelymul, ptr %b_thunk
      
      %1 = call i32 @go(%f_thunk)

    }
