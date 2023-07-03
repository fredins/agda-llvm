target triple = "x86_64-pc-linux-gnu"
target datalayout = "p:64:64:64"


declare void @printf(ptr, ...)
declare ptr @malloc(i64)
@pp_int = private constant [12 x i8] c"result: %d\0A\00", align 1
@pp_debug_int = private constant [25 x i8] c"debug number %d int: %d\0A\00", align 1
@pp_debug_ptr = private constant [25 x i8] c"debug number %d ptr: %d\0A\00", align 1

%Node = type [4 x i64]


define fastcc %Node
@eval(ptr %a){
  %1 = load %Node, ptr %a
  %2 = extractvalue %Node %1, 0
  switch i64 %2, label %clause_def [ i64 0, label %clause_1
                                     i64 1, label %clause_2
                                     i64 2, label %clause_3
                                     i64 3, label %clause_4
                                     i64 4, label %clause_5 ]
clause_1: ; tag Cnat
  ret %Node %1
clause_2: ; tag Cnil
  ret %Node %1
clause_3: ; tag Ccons
  ret %Node %1
clause_4: ; tag FdownFrom
  %3 = extractvalue %Node %1, 1
  %4 = inttoptr i64 %3 to ptr
  %5 = call fastcc %Node @DownFrom.downFrom(ptr %4)
  store %Node %5, ptr %a
  ret %Node %5
clause_5: ; tag Fsum
  %6 = extractvalue %Node %1, 1
  %7 = inttoptr i64 %6 to ptr
  %8 = call fastcc %Node @DownFrom.sum(ptr %7)
  store %Node %8, ptr %a
  ret %Node %8
clause_def:
  call void @printf(ptr @pp_debug_int, i64 1, i64 %2)
  unreachable
}

define fastcc %Node
@DownFrom.downFrom(ptr %a){
  ; eval @0 ; λ Cnat #1 →
  %1 = call fastcc %Node @eval(ptr %a)
  %2 = extractvalue %Node %1, 1

  ; case @0 of
  switch i64 %2, label %clause_def [ i64 0, label %clause_1 ]
clause_1:
  ; unit (C[])
  ret %Node [i64 1, i64 0, i64 0, i64 0]
clause_def:
  ; PSub64 @0 1 ; λ #1 →
  %3 = sub i64 %2, 1

  ; store (Cnat @0) ; λ #1 →
  %4 = call ptr @malloc(i64 32)
  %5 = insertvalue %Node undef, i64 0, 0
  %6 = insertvalue %Node %5, i64 %3, 1
  store %Node %6, ptr %4


  ; store (FdownFrom @0) ; λ #1 →
  %7 = call ptr @malloc(i64 32)
  %8 = insertvalue %Node undef, i64 3, 0
  %9 = ptrtoint ptr %4 to i64
  %10 = insertvalue %Node %8, i64 %9, 1
  store %Node %10, ptr %7

  ; store (Cnat @2) ; λ #1 →
  %11 = call ptr @malloc(i64 32)
  %12 = insertvalue %Node undef, i64 0, 0
  %13 = insertvalue %Node %12, i64 %2, 1
  store %Node %13, ptr %11

  ; unit (C_∷_ @0 @1)
  %14 = insertvalue %Node undef, i64 2, 0
  %15 = ptrtoint ptr %11 to i64
  %16 = insertvalue %Node %14, i64 %15, 1
  %17 = ptrtoint ptr %7 to i64
  %18 = insertvalue %Node %16, i64 %17, 2
  ret %Node %18
}

define fastcc %Node
@DownFrom.sum(ptr %a){
  ; eval @0 ; λ #1 →
  %1 = call fastcc %Node @eval(ptr %a)
  %2 = extractvalue %Node %1, 0 

  ; case @0 of
  switch i64 %2, label %clause_def [ i64 1, label %clause_1
                                     i64 2, label %clause_2 ]
clause_1:
  ; unit (Cnat 0)
  ret %Node [i64 0, i64 0, i64 0, i64 0]
clause_2:
  %3 = extractvalue %Node %1, 1
  %4 = extractvalue %Node %1, 2

  ; eval @1 ; λ Cnat #1 →
  %5 = inttoptr i64 %3 to ptr
  %6 = call fastcc %Node @eval(ptr %5)
  %7 = extractvalue %Node %6, 1

  ; sum @1 ; λ Cnat #1 →
  %8 = inttoptr i64 %4 to ptr
  %9 = call fastcc %Node @DownFrom.sum(ptr %8)
  %10 = extractvalue %Node %9, 1

  ; PAdd64 @1 @0 ; λ #1 →
  %11 = add i64 %7, %10

  ; unit (Cnat @0)
  %12 = insertvalue %Node undef, i64 0, 0
  %13 = insertvalue %Node %12, i64 %11, 1
  ret %Node %13
clause_def:
  unreachable
}

define fastcc void
@main(){
  ; store (Cnat 4) ; λ #1 →
  %1 = call ptr @malloc(i64 32)
  %2 = insertvalue %Node undef, i64 0, 0 ; tag Cint
  %3 = insertvalue %Node %2, i64 4, 1
  store %Node %3, ptr %1

  ; store (FdownFrom @0) ; λ #1 → 
  %4 = call ptr @malloc(i64 32)
  %5 = insertvalue %Node undef, i64 3, 0 ; tag FdownFrom
  %6 = ptrtoint ptr %1 to i64
  %7 = insertvalue %Node %5, i64 %6, 1
  store %Node %7, ptr %4

  ; store (Fsum @0) ; λ #1 →
  %8 = call ptr @malloc(i64 32)
  %9 = insertvalue %Node undef, i64 4, 0 ; tag Fsum
  %10 = ptrtoint ptr %4 to i64
  %11 = insertvalue %Node %9, i64 %10, 1
  store %Node %11, ptr %8

  %12 = call fastcc %Node @eval(ptr %8)
  %13 = extractvalue %Node %12, 1
  call void @printf(ptr @pp_int, i64 %13)
  ret void
}
