target triple = "x86_64-unknown-linux-gnu"
declare void @printf(ptr, ...)
declare ptr @malloc(i64)
declare void @free(ptr)
%Node = type [4 x i64]
@"%d" = private constant [4 x i8] c"%d\0A\00", align 1

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
  %x60 = load i64, ptr %2
  switch i64 %x60, label %default_0 [i64 0, label %"Cnat_0"
                                     i64 1, label %"FAgda.Builtin.Nat._-__0"]
"Cnat_0":
  %3 = inttoptr i64 %x8 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 2
  %x76 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 0
  %6 = insertvalue %Node %5, i64 1, 1
  %"Cnat_0_res" = insertvalue %Node %6, i64 %x76, 2
  br label %continue_0
"FAgda.Builtin.Nat._-__0":
  %7 = inttoptr i64 %x8 to ptr
  %8 = getelementptr inbounds %Node, ptr %7, i32 0, i64 2
  %x77 = load i64, ptr %8
  %9 = inttoptr i64 %x8 to ptr
  %10 = getelementptr inbounds %Node, ptr %9, i32 0, i64 3
  %x78 = load i64, ptr %10
  %"FAgda.Builtin.Nat._-__0_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x77, i64 %x78)
  br label %continue_0
default_0:
  unreachable
continue_0:
  %11 = phi %Node  [%"Cnat_0_res", %"Cnat_0"]
                 , [%"FAgda.Builtin.Nat._-__0_res", %"FAgda.Builtin.Nat._-__0"]
  %x59 = extractvalue %Node %11, 2
  %12 = insertvalue %Node undef, i64 0, 0
  %13 = insertvalue %Node %12, i64 1, 1
  %14 = insertvalue %Node %13, i64 %x59, 2
  %15 = inttoptr i64 %x8 to ptr
  store %Node %14, ptr %15
  switch i64 %x59, label %default_1 [i64 0, label %"0_1"]
"0_1":
  %16 = insertvalue %Node undef, i64 2, 0
  %17 = insertvalue %Node %16, i64 1, 1
  %18 = insertvalue %Node %17, i64 2, 2
  ret %Node %18
default_1:
  %19 = insertvalue %Node undef, i64 0, 0
  %20 = insertvalue %Node %19, i64 1, 1
  %21 = insertvalue %Node %20, i64 1, 2
  %22 = call fastcc ptr @malloc(i64 192)
  store %Node %21, ptr %22
  %x3 = ptrtoint ptr %22 to i64
  %23 = insertvalue %Node undef, i64 1, 0
  %24 = insertvalue %Node %23, i64 1, 1
  %25 = insertvalue %Node %24, i64 %x8, 2
  %26 = insertvalue %Node %25, i64 %x3, 3
  %27 = call fastcc ptr @malloc(i64 192)
  store %Node %26, ptr %27
  %x2 = ptrtoint ptr %27 to i64
  %28 = insertvalue %Node undef, i64 3, 0
  %29 = insertvalue %Node %28, i64 1, 1
  %30 = insertvalue %Node %29, i64 %x2, 2
  %31 = call fastcc ptr @malloc(i64 192)
  store %Node %30, ptr %31
  %x5 = ptrtoint ptr %31 to i64
  %32 = insertvalue %Node undef, i64 4, 0
  %33 = insertvalue %Node %32, i64 1, 1
  %34 = insertvalue %Node %33, i64 %x2, 2
  %35 = insertvalue %Node %34, i64 %x5, 3
  ret %Node %35
}

define fastcc %Node
@"DownFrom.sum"(i64 %x16){
  %1 = inttoptr i64 %x16 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 2
  %x44 = load i64, ptr %2
  %3 = call fastcc %Node @"DownFrom.downFrom"(i64 %x44)
  %x63 = extractvalue %Node %3, 0
  %x64 = extractvalue %Node %3, 2
  %x65 = extractvalue %Node %3, 3
  switch i64 %x63, label %default_2 [i64 2, label %"CDownFrom.List.[]_2"
                                     i64 4, label %"CDownFrom.List._∷__2"]
"CDownFrom.List.[]_2":
  %4 = insertvalue %Node undef, i64 %x63, 0
  %5 = insertvalue %Node %4, i64 1, 1
  %6 = insertvalue %Node %5, i64 %x64, 2
  %7 = insertvalue %Node %6, i64 %x65, 3
  %8 = inttoptr i64 %x16 to ptr
  store %Node %7, ptr %8
  %9 = insertvalue %Node undef, i64 0, 0
  %10 = insertvalue %Node %9, i64 1, 1
  %11 = insertvalue %Node %10, i64 0, 2
  ret %Node %11
"CDownFrom.List._∷__2":
  %12 = insertvalue %Node undef, i64 %x63, 0
  %13 = insertvalue %Node %12, i64 1, 1
  %14 = insertvalue %Node %13, i64 %x64, 2
  %15 = insertvalue %Node %14, i64 %x65, 3
  %16 = inttoptr i64 %x16 to ptr
  store %Node %15, ptr %16
  %17 = insertvalue %Node undef, i64 5, 0
  %18 = insertvalue %Node %17, i64 1, 1
  %19 = insertvalue %Node %18, i64 %x65, 2
  %20 = call fastcc ptr @malloc(i64 192)
  store %Node %19, ptr %20
  %x11 = ptrtoint ptr %20 to i64
  %21 = call fastcc %Node @"Agda.Builtin.Nat._+_"(i64 %x11, i64 %x64)
  ret %Node %21
default_2:
  unreachable
}

define fastcc void
@main(){
  %1 = insertvalue %Node undef, i64 0, 0
  %2 = insertvalue %Node %1, i64 1, 1
  %3 = insertvalue %Node %2, i64 100, 2
  %4 = call fastcc ptr @malloc(i64 192)
  store %Node %3, ptr %4
  %x21 = ptrtoint ptr %4 to i64
  %5 = insertvalue %Node undef, i64 3, 0
  %6 = insertvalue %Node %5, i64 1, 1
  %7 = insertvalue %Node %6, i64 %x21, 2
  %8 = call fastcc ptr @malloc(i64 192)
  store %Node %7, ptr %8
  %x20 = ptrtoint ptr %8 to i64
  %9 = call fastcc %Node @"DownFrom.sum"(i64 %x20)
  %x22 = extractvalue %Node %9, 2
  call fastcc void @printf(ptr @"%d", i64 %x22)
  ret void
}

define fastcc %Node
@"Agda.Builtin.Nat._+_"(i64 %x28, i64 %x29){
  %1 = inttoptr i64 %x28 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 2
  %x46 = load i64, ptr %2
  %3 = call fastcc %Node @"DownFrom.sum"(i64 %x46)
  %x70 = extractvalue %Node %3, 2
  %4 = insertvalue %Node undef, i64 0, 0
  %5 = insertvalue %Node %4, i64 1, 1
  %6 = insertvalue %Node %5, i64 %x70, 2
  %7 = inttoptr i64 %x28 to ptr
  store %Node %6, ptr %7
  %8 = inttoptr i64 %x29 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 0
  %x67 = load i64, ptr %9
  switch i64 %x67, label %default_3 [i64 0, label %"Cnat_3"
                                     i64 1, label %"FAgda.Builtin.Nat._-__3"]
"Cnat_3":
  %10 = inttoptr i64 %x29 to ptr
  %11 = getelementptr inbounds %Node, ptr %10, i32 0, i64 2
  %x79 = load i64, ptr %11
  %12 = insertvalue %Node undef, i64 0, 0
  %13 = insertvalue %Node %12, i64 1, 1
  %"Cnat_3_res" = insertvalue %Node %13, i64 %x79, 2
  br label %continue_3
"FAgda.Builtin.Nat._-__3":
  %14 = inttoptr i64 %x29 to ptr
  %15 = getelementptr inbounds %Node, ptr %14, i32 0, i64 2
  %x80 = load i64, ptr %15
  %16 = inttoptr i64 %x29 to ptr
  %17 = getelementptr inbounds %Node, ptr %16, i32 0, i64 3
  %x81 = load i64, ptr %17
  %"FAgda.Builtin.Nat._-__3_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x80, i64 %x81)
  br label %continue_3
default_3:
  unreachable
continue_3:
  %18 = phi %Node  [%"Cnat_3_res", %"Cnat_3"]
                 , [%"FAgda.Builtin.Nat._-__3_res", %"FAgda.Builtin.Nat._-__3"]
  %x66 = extractvalue %Node %18, 2
  %19 = insertvalue %Node undef, i64 0, 0
  %20 = insertvalue %Node %19, i64 1, 1
  %21 = insertvalue %Node %20, i64 %x66, 2
  %22 = inttoptr i64 %x29 to ptr
  store %Node %21, ptr %22
  %x25 = add i64 %x70, %x66
  %23 = insertvalue %Node undef, i64 0, 0
  %24 = insertvalue %Node %23, i64 1, 1
  %25 = insertvalue %Node %24, i64 %x25, 2
  ret %Node %25
}

define fastcc %Node
@"Agda.Builtin.Nat._-_"(i64 %x35, i64 %x36){
  %1 = inttoptr i64 %x35 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x73 = load i64, ptr %2
  switch i64 %x73, label %default_4 [i64 0, label %"Cnat_4"
                                     i64 1, label %"FAgda.Builtin.Nat._-__4"]
"Cnat_4":
  %3 = inttoptr i64 %x35 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 2
  %x82 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 0
  %6 = insertvalue %Node %5, i64 1, 1
  %"Cnat_4_res" = insertvalue %Node %6, i64 %x82, 2
  br label %continue_4
"FAgda.Builtin.Nat._-__4":
  %7 = inttoptr i64 %x35 to ptr
  %8 = getelementptr inbounds %Node, ptr %7, i32 0, i64 2
  %x83 = load i64, ptr %8
  %9 = inttoptr i64 %x35 to ptr
  %10 = getelementptr inbounds %Node, ptr %9, i32 0, i64 3
  %x84 = load i64, ptr %10
  %"FAgda.Builtin.Nat._-__4_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x83, i64 %x84)
  br label %continue_4
default_4:
  unreachable
continue_4:
  %11 = phi %Node  [%"Cnat_4_res", %"Cnat_4"]
                 , [%"FAgda.Builtin.Nat._-__4_res", %"FAgda.Builtin.Nat._-__4"]
  %x72 = extractvalue %Node %11, 2
  %12 = insertvalue %Node undef, i64 0, 0
  %13 = insertvalue %Node %12, i64 1, 1
  %14 = insertvalue %Node %13, i64 %x72, 2
  %15 = inttoptr i64 %x35 to ptr
  store %Node %14, ptr %15
  %16 = inttoptr i64 %x36 to ptr
  %17 = getelementptr inbounds %Node, ptr %16, i32 0, i64 2
  %x58 = load i64, ptr %17
  %18 = insertvalue %Node undef, i64 0, 0
  %19 = insertvalue %Node %18, i64 1, 1
  %20 = insertvalue %Node %19, i64 %x58, 2
  %21 = inttoptr i64 %x36 to ptr
  store %Node %20, ptr %21
  %x32 = sub i64 %x72, %x58
  %22 = insertvalue %Node undef, i64 0, 0
  %23 = insertvalue %Node %22, i64 1, 1
  %24 = insertvalue %Node %23, i64 %x32, 2
  ret %Node %24
}

define fastcc void
@"drop"(i64 %x94){
  %1 = inttoptr i64 %x94 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x93 = load i64, ptr %2
  switch i64 %x93, label %default_5 [i64 0, label %"0_5"]
"0_5":
  %3 = inttoptr i64 %x94 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 0
  %x92 = load i64, ptr %4
  switch i64 %x92, label %default_6 [i64 2, label %"CDownFrom.List.[]_6"
                                     i64 4, label %"CDownFrom.List._∷__6"
                                     i64 0, label %"Cnat_6"
                                     i64 1, label %"FAgda.Builtin.Nat._-__6"
                                     i64 3, label %"FDownFrom.downFrom_6"
                                     i64 5, label %"FDownFrom.sum_6"]
"CDownFrom.List.[]_6":
  %5 = inttoptr i64 %x94 to ptr
  call fastcc void @free(ptr %5)
  ret void
"CDownFrom.List._∷__6":
  %6 = inttoptr i64 %x94 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x86 = load i64, ptr %7
  %8 = inttoptr i64 %x94 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x85 = load i64, ptr %9
  %10 = inttoptr i64 %x94 to ptr
  call fastcc void @free(ptr %10)
  ret void
"Cnat_6":
  %11 = inttoptr i64 %x94 to ptr
  %12 = getelementptr inbounds %Node, ptr %11, i32 0, i64 2
  %x87 = load i64, ptr %12
  %13 = inttoptr i64 %x94 to ptr
  call fastcc void @free(ptr %13)
  ret void
"FAgda.Builtin.Nat._-__6":
  %14 = inttoptr i64 %x94 to ptr
  %15 = getelementptr inbounds %Node, ptr %14, i32 0, i64 2
  %x89 = load i64, ptr %15
  %16 = inttoptr i64 %x94 to ptr
  %17 = getelementptr inbounds %Node, ptr %16, i32 0, i64 3
  %x88 = load i64, ptr %17
  %18 = inttoptr i64 %x94 to ptr
  call fastcc void @free(ptr %18)
  ret void
"FDownFrom.downFrom_6":
  %19 = inttoptr i64 %x94 to ptr
  %20 = getelementptr inbounds %Node, ptr %19, i32 0, i64 2
  %x90 = load i64, ptr %20
  %21 = inttoptr i64 %x94 to ptr
  call fastcc void @free(ptr %21)
  ret void
"FDownFrom.sum_6":
  %22 = inttoptr i64 %x94 to ptr
  %23 = getelementptr inbounds %Node, ptr %22, i32 0, i64 2
  %x91 = load i64, ptr %23
  %24 = inttoptr i64 %x94 to ptr
  call fastcc void @free(ptr %24)
  ret void
default_6:
  unreachable
default_5:
  %25 = inttoptr i64 %x93 to ptr
  %26 = getelementptr inbounds %Node, ptr %25, i32 0, i64 0
  %27 = load i64, ptr %26
  %28 = sub i64 %27, 1
  store i64 %28, ptr %25
  ret void
}