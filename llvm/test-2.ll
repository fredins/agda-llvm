target triple = "x86_64-pc-linux-gnu"

;; pretty printing
;; libc
declare ptr @malloc(i64)
declare void @free(ptr)
declare void @printf(ptr, ...)

; TODO move some information prefix function ptr
%Info = type
  { i1   ; evaluated?
  , i8   ; reference count
  , i8   ; next position
  , i8   ; arity
  , ptr  ; function
  , i64  ; size
  }

%List = type { i8, ptr, ptr }

define ptr 
@cons(ptr %a, ptr %b){
  %1 = call ptr @malloc(i64 24)
  %2 = getelementptr inbounds %List, ptr %1, i32 0, i32 0
  store i8 1, ptr %2
  %3 = getelementptr inbounds %List, ptr %1, i32 0, i32 1
  store ptr %a, ptr %3
  %4 = getelementptr inbounds %List, ptr %1, i32 0, i32 2
  store ptr %b, ptr %4
  ret ptr %1
}

define ptr 
@nil(){
  %1 = call ptr @malloc(i64 8) 
  store i8 0, ptr %1
  ret ptr %1
}

define ptr 
@head(ptr %thunk){
  ; FORCE THUNK
  %1 = getelementptr { ptr, ptr, ptr }, ptr %thunk, i32 0, i32 0
  %2 = getelementptr { ptr, ptr, ptr }, ptr %thunk, i32 0, i32 1
  %3 = getelementptr { ptr, ptr, ptr }, ptr %thunk, i32 0, i32 2
  %4 = load ptr, ptr %1
  %5 = load ptr, ptr %2
  %6 = load ptr, ptr %3
  %7 = call ptr %4(ptr %5, ptr %6)

  ; MATCH ON TAG
  %8 = getelementptr %List, ptr %7, i32 0, i32 0
  %9 = load i8, ptr %8
  call void @printf(ptr @pp_int, i8 %9)

  ; GET HEAD THUNK
  %10 = getelementptr %List, ptr %7, i32 0, i32 1
  %11 = load ptr, ptr %10

  ret ptr %11
}

define i64 
@main(){
  %1 = call ptr @malloc(i64 24)

  ; INSERT FUNCTION
  %2 = getelementptr { ptr, ptr, ptr }, ptr %1, i32 0, i32 0
  store ptr @cons, ptr %2

  ; INSERT HEAD THUNK
  %3 = call ptr @malloc(i64 64)
  store i64 1600, ptr %3
  %4 = getelementptr { ptr, ptr, ptr }, ptr %1, i32 0, i32 1
  store ptr %3, ptr %4

  ; INSERT TAIL THUNK
  %5 = call ptr @nil()
  %6 = getelementptr { ptr, ptr, ptr }, ptr %1, i32 0, i32 2
  store ptr %5, ptr %6

  %7 = call ptr @head(ptr %1)

  ; FORCE 
  %8 = load i64, ptr %7
  call void @printf(ptr @pp_int, i64 %8)
  ret i64 0
}

define void
@mkInfo(ptr %thunk, i1 %eval, i8 %refc, i8 %next, i8 %arity, ptr %fun, i64 %size){
  %1 = getelementptr inbounds { %Info, ptr, ptr } , ptr %thunk, i32 0, i32 0, i32 0
  store i1 %eval, ptr %1
  %2 = getelementptr inbounds { %Info, ptr, ptr } , ptr %thunk, i32 0, i32 0, i32 1
  store i8 %refc, ptr %2
  %3 = getelementptr inbounds { %Info, ptr, ptr } , ptr %thunk, i32 0, i32 0, i32 2
  store i8 %next, ptr %3
  %4 = getelementptr inbounds { %Info, ptr, ptr } , ptr %thunk, i32 0, i32 0, i32 3
  store i8 %arity, ptr %4
  %5 = getelementptr inbounds { %Info, ptr, ptr } , ptr %thunk, i32 0, i32 0, i32 4
  store ptr %fun, ptr %5
  %6 = getelementptr inbounds { %Info, ptr, ptr } , ptr %thunk, i32 0, i32 0, i32 5
  store i64 %size, ptr %6
  ret void
}

@pp_eval  = private constant [16 x i8] c"evaluated = %d\0A\00", align 1
@pp_refc  = private constant [16 x i8] c"ref count = %d\0A\00", align 1
@pp_next  = private constant [16 x i8] c"next pos  = %d\0A\00", align 1
@pp_fun   = private constant [16 x i8] c"function  = %p\0A\00", align 1
@pp_size  = private constant [16 x i8] c"size      = %d\0A\00", align 1
@pp_arity = private constant [16 x i8] c"arity     = %d\0A\00", align 1

define void 
@ppInfo(ptr %thunk){
  %1 = getelementptr inbounds { %Info } , ptr %thunk, i32 0, i32 0, i32 0
  %2 = load i1, ptr %1
  call void @printf(ptr @pp_eval, i1 %2)
  %3 = getelementptr inbounds { %Info } , ptr %thunk, i32 0, i32 0, i32 1
  %4 = load i8, ptr %3
  call void @printf(ptr @pp_refc, i8 %4)
  %5 = getelementptr inbounds { %Info } , ptr %thunk, i32 0, i32 0, i32 2
  %6 = load i8, ptr %5
  call void @printf(ptr @pp_next, i8 %6)
  %7 = getelementptr inbounds { %Info } , ptr %thunk, i32 0, i32 0, i32 3
  %8 = load i8, ptr %7
  call void @printf(ptr @pp_arity, i8 %8)
  %9 = getelementptr inbounds { %Info } , ptr %thunk, i32 0, i32 0, i32 4
  %10 = load ptr, ptr %9
  call void @printf(ptr @pp_fun, ptr %10)
  %11 = getelementptr inbounds { %Info } , ptr %thunk, i32 0, i32 0, i32 5
  %12 = load i64, ptr %11
  call void @printf(ptr @pp_size, i64 %12)
  ret void 
}
