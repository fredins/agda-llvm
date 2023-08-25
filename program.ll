target triple = "x86_64-unknown-linux-gnu"
declare void @printf(ptr, ...)
declare ptr @malloc(i64)
%Node = type [3 x i64]
@"%d" = private constant [4 x i8] c"%d\0A\00", align 1
@"sum" = private constant [9 x i8] c"sum: %d\0A\00", align  1
@"downFrom" = private constant [14 x i8] c"downFrom: %d\0A\00", align 1
@"_-_" = private constant [9 x i8] c"_-_: %d\0A\00", align 1
@"_+_" = private constant [9 x i8] c"_+_: %d\0A\00", align 1


; Tag numbering table:
; 0 Cnat
; 1 FAgda.Builtin.Nat._-_
; 2 CDownFrom.List.[]
; 3 FDownFrom.downFrom
; 4 CDownFrom.List._∷_
; 5 FDownFrom.sum

define fastcc %Node
@"DownFrom.downFrom"(i64 %x8){
  %1 = inttoptr i64 %x8 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x1668 = load i64, ptr %2
  switch i64 %x1668, label %default_0 [ i64 0, label %"Cnat_0"
                                        i64 1, label %"FAgda.Builtin.Nat._-__0" ]
"Cnat_0":
  %3 = inttoptr i64 %x8 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 1
  %x3292 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 0
  %"Cnat_0_res" = insertvalue %Node %5, i64 %x3292, 1
  br label %continue_0
"FAgda.Builtin.Nat._-__0":
  %6 = inttoptr i64 %x8 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 1
  %x3293 = load i64, ptr %7
  %8 = inttoptr i64 %x8 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 2
  %x3294 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__0_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x3293, i64 %x3294)
  br label %continue_0
default_0:
  unreachable
continue_0:
  %10 = phi %Node [%"Cnat_0_res", %"Cnat_0"], [%"FAgda.Builtin.Nat._-__0_res", %"FAgda.Builtin.Nat._-__0"]
  %x1667 = extractvalue %Node %10, 1
  %11 = insertvalue %Node undef, i64 0, 0
  %12 = insertvalue %Node %11, i64 %x1667, 1
  %13 = inttoptr i64 %x8 to ptr
  store %Node %12, ptr %13
  switch i64 %x1667, label %default_1 [ i64 0, label %"0_1" ]
"0_1":
  %14 = insertvalue %Node undef, i64 2, 0
  ret %Node %14
default_1:
  %15 = insertvalue %Node undef, i64 0, 0
  %16 = insertvalue %Node %15, i64 1, 1
  %17 = call fastcc ptr @malloc(i64 192)
  store %Node %16, ptr %17
  %x3 = ptrtoint ptr %17 to i64
  %18 = insertvalue %Node undef, i64 1, 0
  %19 = insertvalue %Node %18, i64 %x8, 1
  %20 = insertvalue %Node %19, i64 %x3, 2
  %21 = call fastcc ptr @malloc(i64 192)
  store %Node %20, ptr %21
  %x2 = ptrtoint ptr %21 to i64
  %22 = insertvalue %Node undef, i64 3, 0
  %23 = insertvalue %Node %22, i64 %x2, 1
  %24 = call fastcc ptr @malloc(i64 192)
  store %Node %23, ptr %24
  %x5 = ptrtoint ptr %24 to i64
  %25 = insertvalue %Node undef, i64 4, 0
  %26 = insertvalue %Node %25, i64 %x2, 1
  %27 = insertvalue %Node %26, i64 %x5, 2
  ret %Node %27
}

define fastcc %Node
@"DownFrom.sum"(i64 %x16){
  %1 = inttoptr i64 %x16 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x44 = load i64, ptr %2
  %3 = call fastcc %Node @"DownFrom.downFrom"(i64 %x44)
  %x1671 = extractvalue %Node %3, 0
  %x1672 = extractvalue %Node %3, 1
  %x1673 = extractvalue %Node %3, 2
  switch i64 %x1671, label %default_2 [ i64 2, label %"CDownFrom.List.[]_2"
                                        i64 4, label %"CDownFrom.List._∷__2" ]
"CDownFrom.List.[]_2":
  %4 = insertvalue %Node undef, i64 %x1671, 0
  %5 = insertvalue %Node %4, i64 %x1672, 1
  %6 = insertvalue %Node %5, i64 %x1673, 2
  %7 = inttoptr i64 %x16 to ptr
  store %Node %6, ptr %7
  %8 = insertvalue %Node undef, i64 0, 0
  %9 = insertvalue %Node %8, i64 0, 1
  ret %Node %9
"CDownFrom.List._∷__2":
  %10 = insertvalue %Node undef, i64 %x1671, 0
  %11 = insertvalue %Node %10, i64 %x1672, 1
  %12 = insertvalue %Node %11, i64 %x1673, 2
  %13 = inttoptr i64 %x16 to ptr
  store %Node %12, ptr %13
  %14 = insertvalue %Node undef, i64 5, 0
  %15 = insertvalue %Node %14, i64 %x1673, 1
  %16 = call fastcc ptr @malloc(i64 192)
  store %Node %15, ptr %16
  %x11 = ptrtoint ptr %16 to i64
  %17 = call
        fastcc
        %Node
        @"Agda.Builtin.Nat._+_"(i64 %x11, i64 %x1672)
  ret %Node %17
default_2:
  unreachable
}

define fastcc void
@main(){
  %1 = insertvalue %Node undef, i64 0, 0
  %2 = insertvalue %Node %1, i64 100, 1
  %3 = call fastcc ptr @malloc(i64 192)
  store %Node %2, ptr %3
  %x21 = ptrtoint ptr %3 to i64
  %4 = insertvalue %Node undef, i64 3, 0
  %5 = insertvalue %Node %4, i64 %x21, 1
  %6 = call fastcc ptr @malloc(i64 192)
  store %Node %5, ptr %6
  %x20 = ptrtoint ptr %6 to i64
  %7 = call fastcc %Node @"DownFrom.sum"(i64 %x20)
  %x22 = extractvalue %Node %7, 1
  call fastcc void @printf(ptr @"%d", i64 %x22)
  ret void
}

define fastcc %Node
@"Agda.Builtin.Nat._+_"(i64 %x28, i64 %x29){
  %1 = inttoptr i64 %x28 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x46 = load i64, ptr %2
  %3 = call fastcc %Node @"DownFrom.sum"(i64 %x46)
  %x1678 = extractvalue %Node %3, 1
  %4 = insertvalue %Node undef, i64 0, 0
  %5 = insertvalue %Node %4, i64 %x1678, 1
  %6 = inttoptr i64 %x28 to ptr
  store %Node %5, ptr %6
  %7 = inttoptr i64 %x29 to ptr
  %8 = getelementptr inbounds %Node, ptr %7, i32 0, i64 0
  %x1675 = load i64, ptr %8
  switch i64 %x1675, label %default_3 [ i64 0, label %"Cnat_3"
                                        i64 1, label %"FAgda.Builtin.Nat._-__3" ]
"Cnat_3":
  %9 = inttoptr i64 %x29 to ptr
  %10 = getelementptr inbounds %Node, ptr %9, i32 0, i64 1
  %x3295 = load i64, ptr %10
  %11 = insertvalue %Node undef, i64 0, 0
  %"Cnat_3_res" = insertvalue %Node %11, i64 %x3295, 1
  br label %continue_3
"FAgda.Builtin.Nat._-__3":
  %12 = inttoptr i64 %x29 to ptr
  %13 = getelementptr inbounds %Node, ptr %12, i32 0, i64 1
  %x3296 = load i64, ptr %13
  %14 = inttoptr i64 %x29 to ptr
  %15 = getelementptr inbounds %Node, ptr %14, i32 0, i64 2
  %x3297 = load i64, ptr %15
  %"FAgda.Builtin.Nat._-__3_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x3296, i64 %x3297)
  br label %continue_3
default_3:
  unreachable
continue_3:
  %16 = phi %Node [%"Cnat_3_res", %"Cnat_3"], [%"FAgda.Builtin.Nat._-__3_res", %"FAgda.Builtin.Nat._-__3"]
  %x1674 = extractvalue %Node %16, 1
  %17 = insertvalue %Node undef, i64 0, 0
  %18 = insertvalue %Node %17, i64 %x1674, 1
  %19 = inttoptr i64 %x29 to ptr
  store %Node %18, ptr %19
  %x25 = add i64 %x1678, %x1674
  %20 = insertvalue %Node undef, i64 0, 0
  %21 = insertvalue %Node %20, i64 %x25, 1
  ret %Node %21
}

define fastcc %Node
@"Agda.Builtin.Nat._-_"(i64 %x35, i64 %x36){
  %1 = inttoptr i64 %x35 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x1681 = load i64, ptr %2
  switch i64 %x1681, label %default_4 [ i64 0, label %"Cnat_4"
                                        i64 1, label %"FAgda.Builtin.Nat._-__4" ]
"Cnat_4":
  %3 = inttoptr i64 %x35 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 1
  %x3298 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 0
  %"Cnat_4_res" = insertvalue %Node %5, i64 %x3298, 1
  br label %continue_4
"FAgda.Builtin.Nat._-__4":
  %6 = inttoptr i64 %x35 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 1
  %x3299 = load i64, ptr %7
  %8 = inttoptr i64 %x35 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 2
  %x3300 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__4_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x3299, i64 %x3300)
  br label %continue_4
default_4:
  unreachable
continue_4:
  %10 = phi %Node [%"Cnat_4_res", %"Cnat_4"], [%"FAgda.Builtin.Nat._-__4_res", %"FAgda.Builtin.Nat._-__4"]
  %x1680 = extractvalue %Node %10, 1
  %11 = insertvalue %Node undef, i64 0, 0
  %12 = insertvalue %Node %11, i64 %x1680, 1
  %13 = inttoptr i64 %x35 to ptr
  store %Node %12, ptr %13
  %14 = inttoptr i64 %x36 to ptr
  %15 = getelementptr inbounds %Node, ptr %14, i32 0, i64 1
  %x58 = load i64, ptr %15
  %16 = insertvalue %Node undef, i64 0, 0
  %17 = insertvalue %Node %16, i64 %x58, 1
  %18 = inttoptr i64 %x36 to ptr
  store %Node %17, ptr %18
  %x32 = sub i64 %x1680, %x58
  %19 = insertvalue %Node undef, i64 0, 0
  %20 = insertvalue %Node %19, i64 %x32, 1
  ret %Node %20
}