target triple = "x86_64-pc-linux-gnu"

;; pretty printing
@pp_int = private constant [4 x i8] c"%d\0A\00", align 1

;; libc
declare ptr @malloc(i64)
declare void @free(ptr)
declare void @printf(ptr, ...)

;; types
%Info = type 
  { i1  ; evaluated
  , ptr ; function
  }

define i64 @main(){
  call void @printList(ptr null)
  ret i64 0
}

@pp_left = internal constant [2 x i8] c"[\00"
@pp_right = internal constant [2 x i8] c"]\00"
@pp_d = internal constant [3 x i8] c"%d\00"
define void @printList(ptr %thunk){
  call void @printf(ptr @pp_left)



  call void @printf(ptr @pp_right)
  ret void
}


define void evalList(ptr %thunk){
  %1 = getelementptr inbounds { %Info }, ptr %thunk, i32 0, i32 0, i32 0
  %2 = load i1, ptr %1
  br i1 %2, label %evaluated, label %unevaluated
unevaluated:
  %3 = getelementptr inbounds { %Info, ptr, ptr }, ptr %thunk, i32 0, i32 0, i32 1
  %4 = getelementptr inbounds { %Info, ptr, ptr }, ptr %thunk, i32 0, i32 1
  %5 = getelementptr inbounds { %Info, ptr, ptr }, ptr %thunk, i32 0, i32 2
  call ptr %3(ptr %4, ptr %5)
evaluated: 

}
