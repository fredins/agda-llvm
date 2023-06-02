


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
