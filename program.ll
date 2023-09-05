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
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %3 = load i64, ptr %2
  %4 = add i64 %3, 1
  store i64 %4, ptr %1
  %5 = inttoptr i64 %x8 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 0
  %x60 = load i64, ptr %6
  switch i64 %x60, label %default_0 [i64 0, label %"Cnat_0"
                                     i64 1, label %"FAgda.Builtin.Nat._-__0"]
"Cnat_0":
  %7 = inttoptr i64 %x8 to ptr
  %8 = getelementptr inbounds %Node, ptr %7, i32 0, i64 1
  %9 = load i64, ptr %8
  %10 = add i64 %9, 1
  store i64 %10, ptr %7
  %11 = inttoptr i64 %x8 to ptr
  %12 = getelementptr inbounds %Node, ptr %11, i32 0, i64 2
  %x76 = load i64, ptr %12
  %13 = insertvalue %Node undef, i64 0, 0
  %14 = insertvalue %Node %13, i64 1, 1
  %"Cnat_0_res" = insertvalue %Node %14, i64 %x76, 2
  br label %continue_0
"FAgda.Builtin.Nat._-__0":
  %15 = inttoptr i64 %x8 to ptr
  %16 = getelementptr inbounds %Node, ptr %15, i32 0, i64 1
  %17 = load i64, ptr %16
  %18 = add i64 %17, 1
  store i64 %18, ptr %15
  %19 = inttoptr i64 %x8 to ptr
  %20 = getelementptr inbounds %Node, ptr %19, i32 0, i64 2
  %x77 = load i64, ptr %20
  %21 = inttoptr i64 %x8 to ptr
  %22 = getelementptr inbounds %Node, ptr %21, i32 0, i64 1
  %23 = load i64, ptr %22
  %24 = add i64 %23, 1
  store i64 %24, ptr %21
  %25 = inttoptr i64 %x8 to ptr
  %26 = getelementptr inbounds %Node, ptr %25, i32 0, i64 3
  %x78 = load i64, ptr %26
  %"FAgda.Builtin.Nat._-__0_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x77, i64 %x78)
  br label %continue_0
default_0:
  unreachable
continue_0:
  %27 = phi %Node  [%"Cnat_0_res", %"Cnat_0"]
                 , [%"FAgda.Builtin.Nat._-__0_res", %"FAgda.Builtin.Nat._-__0"]
  %x59 = extractvalue %Node %27, 2
  %28 = insertvalue %Node undef, i64 0, 0
  %29 = insertvalue %Node %28, i64 1, 1
  %30 = insertvalue %Node %29, i64 %x59, 2
  %31 = inttoptr i64 %x8 to ptr
  store %Node %30, ptr %31
  switch i64 %x59, label %default_1 [i64 0, label %"0_1"]
"0_1":
  %32 = insertvalue %Node undef, i64 2, 0
  %33 = insertvalue %Node %32, i64 1, 1
  %34 = insertvalue %Node %33, i64 2, 2
  ret %Node %34
default_1:
  %35 = insertvalue %Node undef, i64 0, 0
  %36 = insertvalue %Node %35, i64 1, 1
  %37 = insertvalue %Node %36, i64 1, 2
  %38 = call fastcc ptr @malloc(i64 192)
  store %Node %37, ptr %38
  %x3 = ptrtoint ptr %38 to i64
  %39 = insertvalue %Node undef, i64 1, 0
  %40 = insertvalue %Node %39, i64 1, 1
  %41 = insertvalue %Node %40, i64 %x8, 2
  %42 = insertvalue %Node %41, i64 %x3, 3
  %43 = call fastcc ptr @malloc(i64 192)
  store %Node %42, ptr %43
  %x2 = ptrtoint ptr %43 to i64
  %44 = insertvalue %Node undef, i64 3, 0
  %45 = insertvalue %Node %44, i64 1, 1
  %46 = insertvalue %Node %45, i64 %x2, 2
  %47 = call fastcc ptr @malloc(i64 192)
  store %Node %46, ptr %47
  %x5 = ptrtoint ptr %47 to i64
  %48 = insertvalue %Node undef, i64 4, 0
  %49 = insertvalue %Node %48, i64 1, 1
  %50 = insertvalue %Node %49, i64 %x2, 2
  %51 = insertvalue %Node %50, i64 %x5, 3
  ret %Node %51
}

define fastcc %Node
@"DownFrom.sum"(i64 %x16){
  %1 = inttoptr i64 %x16 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %3 = load i64, ptr %2
  %4 = add i64 %3, 1
  store i64 %4, ptr %1
  %5 = inttoptr i64 %x16 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 2
  %x44 = load i64, ptr %6
  %7 = call fastcc %Node @"DownFrom.downFrom"(i64 %x44)
  %x63 = extractvalue %Node %7, 0
  %x64 = extractvalue %Node %7, 2
  %x65 = extractvalue %Node %7, 3
  switch i64 %x63, label %default_2 [i64 2, label %"CDownFrom.List.[]_2"
                                     i64 4, label %"CDownFrom.List._∷__2"]
"CDownFrom.List.[]_2":
  %8 = inttoptr i64 %x64 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 1
  %10 = load i64, ptr %9
  %11 = add i64 %10, 1
  store i64 %11, ptr %8
  %12 = inttoptr i64 %x65 to ptr
  %13 = getelementptr inbounds %Node, ptr %12, i32 0, i64 1
  %14 = load i64, ptr %13
  %15 = add i64 %14, 1
  store i64 %15, ptr %12
  %16 = insertvalue %Node undef, i64 %x63, 0
  %17 = insertvalue %Node %16, i64 1, 1
  %18 = insertvalue %Node %17, i64 %x64, 2
  %19 = insertvalue %Node %18, i64 %x65, 3
  %20 = inttoptr i64 %x16 to ptr
  store %Node %19, ptr %20
  %21 = insertvalue %Node undef, i64 0, 0
  %22 = insertvalue %Node %21, i64 1, 1
  %23 = insertvalue %Node %22, i64 0, 2
  ret %Node %23
"CDownFrom.List._∷__2":
  %24 = inttoptr i64 %x64 to ptr
  %25 = getelementptr inbounds %Node, ptr %24, i32 0, i64 1
  %26 = load i64, ptr %25
  %27 = add i64 %26, 1
  store i64 %27, ptr %24
  %28 = inttoptr i64 %x65 to ptr
  %29 = getelementptr inbounds %Node, ptr %28, i32 0, i64 1
  %30 = load i64, ptr %29
  %31 = add i64 %30, 1
  store i64 %31, ptr %28
  %32 = insertvalue %Node undef, i64 %x63, 0
  %33 = insertvalue %Node %32, i64 1, 1
  %34 = insertvalue %Node %33, i64 %x64, 2
  %35 = insertvalue %Node %34, i64 %x65, 3
  %36 = inttoptr i64 %x16 to ptr
  store %Node %35, ptr %36
  %37 = insertvalue %Node undef, i64 5, 0
  %38 = insertvalue %Node %37, i64 1, 1
  %39 = insertvalue %Node %38, i64 %x65, 2
  %40 = call fastcc ptr @malloc(i64 192)
  store %Node %39, ptr %40
  %x11 = ptrtoint ptr %40 to i64
  %41 = call fastcc %Node @"Agda.Builtin.Nat._+_"(i64 %x11, i64 %x64)
  ret %Node %41
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
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %3 = load i64, ptr %2
  %4 = add i64 %3, 1
  store i64 %4, ptr %1
  %5 = inttoptr i64 %x28 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 2
  %x46 = load i64, ptr %6
  %7 = call fastcc %Node @"DownFrom.sum"(i64 %x46)
  %x70 = extractvalue %Node %7, 2
  %8 = insertvalue %Node undef, i64 0, 0
  %9 = insertvalue %Node %8, i64 1, 1
  %10 = insertvalue %Node %9, i64 %x70, 2
  %11 = inttoptr i64 %x28 to ptr
  store %Node %10, ptr %11
  %12 = inttoptr i64 %x29 to ptr
  %13 = getelementptr inbounds %Node, ptr %12, i32 0, i64 1
  %14 = load i64, ptr %13
  %15 = add i64 %14, 1
  store i64 %15, ptr %12
  %16 = inttoptr i64 %x29 to ptr
  %17 = getelementptr inbounds %Node, ptr %16, i32 0, i64 0
  %x67 = load i64, ptr %17
  switch i64 %x67, label %default_3 [i64 0, label %"Cnat_3"
                                     i64 1, label %"FAgda.Builtin.Nat._-__3"]
"Cnat_3":
  %18 = inttoptr i64 %x29 to ptr
  %19 = getelementptr inbounds %Node, ptr %18, i32 0, i64 1
  %20 = load i64, ptr %19
  %21 = add i64 %20, 1
  store i64 %21, ptr %18
  %22 = inttoptr i64 %x29 to ptr
  %23 = getelementptr inbounds %Node, ptr %22, i32 0, i64 2
  %x79 = load i64, ptr %23
  %24 = insertvalue %Node undef, i64 0, 0
  %25 = insertvalue %Node %24, i64 1, 1
  %"Cnat_3_res" = insertvalue %Node %25, i64 %x79, 2
  br label %continue_3
"FAgda.Builtin.Nat._-__3":
  %26 = inttoptr i64 %x29 to ptr
  %27 = getelementptr inbounds %Node, ptr %26, i32 0, i64 1
  %28 = load i64, ptr %27
  %29 = add i64 %28, 1
  store i64 %29, ptr %26
  %30 = inttoptr i64 %x29 to ptr
  %31 = getelementptr inbounds %Node, ptr %30, i32 0, i64 2
  %x80 = load i64, ptr %31
  %32 = inttoptr i64 %x29 to ptr
  %33 = getelementptr inbounds %Node, ptr %32, i32 0, i64 1
  %34 = load i64, ptr %33
  %35 = add i64 %34, 1
  store i64 %35, ptr %32
  %36 = inttoptr i64 %x29 to ptr
  %37 = getelementptr inbounds %Node, ptr %36, i32 0, i64 3
  %x81 = load i64, ptr %37
  %"FAgda.Builtin.Nat._-__3_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x80, i64 %x81)
  br label %continue_3
default_3:
  unreachable
continue_3:
  %38 = phi %Node  [%"Cnat_3_res", %"Cnat_3"]
                 , [%"FAgda.Builtin.Nat._-__3_res", %"FAgda.Builtin.Nat._-__3"]
  %x66 = extractvalue %Node %38, 2
  %39 = insertvalue %Node undef, i64 0, 0
  %40 = insertvalue %Node %39, i64 1, 1
  %41 = insertvalue %Node %40, i64 %x66, 2
  %42 = inttoptr i64 %x29 to ptr
  store %Node %41, ptr %42
  %x25 = add i64 %x70, %x66
  %43 = insertvalue %Node undef, i64 0, 0
  %44 = insertvalue %Node %43, i64 1, 1
  %45 = insertvalue %Node %44, i64 %x25, 2
  ret %Node %45
}

define fastcc %Node
@"Agda.Builtin.Nat._-_"(i64 %x35, i64 %x36){
  %1 = inttoptr i64 %x35 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %3 = load i64, ptr %2
  %4 = add i64 %3, 1
  store i64 %4, ptr %1
  %5 = inttoptr i64 %x35 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 0
  %x73 = load i64, ptr %6
  switch i64 %x73, label %default_4 [i64 0, label %"Cnat_4"
                                     i64 1, label %"FAgda.Builtin.Nat._-__4"]
"Cnat_4":
  %7 = inttoptr i64 %x35 to ptr
  %8 = getelementptr inbounds %Node, ptr %7, i32 0, i64 1
  %9 = load i64, ptr %8
  %10 = add i64 %9, 1
  store i64 %10, ptr %7
  %11 = inttoptr i64 %x35 to ptr
  %12 = getelementptr inbounds %Node, ptr %11, i32 0, i64 2
  %x82 = load i64, ptr %12
  %13 = insertvalue %Node undef, i64 0, 0
  %14 = insertvalue %Node %13, i64 1, 1
  %"Cnat_4_res" = insertvalue %Node %14, i64 %x82, 2
  br label %continue_4
"FAgda.Builtin.Nat._-__4":
  %15 = inttoptr i64 %x35 to ptr
  %16 = getelementptr inbounds %Node, ptr %15, i32 0, i64 1
  %17 = load i64, ptr %16
  %18 = add i64 %17, 1
  store i64 %18, ptr %15
  %19 = inttoptr i64 %x35 to ptr
  %20 = getelementptr inbounds %Node, ptr %19, i32 0, i64 2
  %x83 = load i64, ptr %20
  %21 = inttoptr i64 %x35 to ptr
  %22 = getelementptr inbounds %Node, ptr %21, i32 0, i64 1
  %23 = load i64, ptr %22
  %24 = add i64 %23, 1
  store i64 %24, ptr %21
  %25 = inttoptr i64 %x35 to ptr
  %26 = getelementptr inbounds %Node, ptr %25, i32 0, i64 3
  %x84 = load i64, ptr %26
  %"FAgda.Builtin.Nat._-__4_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x83, i64 %x84)
  br label %continue_4
default_4:
  unreachable
continue_4:
  %27 = phi %Node  [%"Cnat_4_res", %"Cnat_4"]
                 , [%"FAgda.Builtin.Nat._-__4_res", %"FAgda.Builtin.Nat._-__4"]
  %x72 = extractvalue %Node %27, 2
  %28 = insertvalue %Node undef, i64 0, 0
  %29 = insertvalue %Node %28, i64 1, 1
  %30 = insertvalue %Node %29, i64 %x72, 2
  %31 = inttoptr i64 %x35 to ptr
  store %Node %30, ptr %31
  %32 = inttoptr i64 %x36 to ptr
  %33 = getelementptr inbounds %Node, ptr %32, i32 0, i64 1
  %34 = load i64, ptr %33
  %35 = add i64 %34, 1
  store i64 %35, ptr %32
  %36 = inttoptr i64 %x36 to ptr
  %37 = getelementptr inbounds %Node, ptr %36, i32 0, i64 2
  %x58 = load i64, ptr %37
  %38 = insertvalue %Node undef, i64 0, 0
  %39 = insertvalue %Node %38, i64 1, 1
  %40 = insertvalue %Node %39, i64 %x58, 2
  %41 = inttoptr i64 %x36 to ptr
  store %Node %40, ptr %41
  %x32 = sub i64 %x72, %x58
  %42 = insertvalue %Node undef, i64 0, 0
  %43 = insertvalue %Node %42, i64 1, 1
  %44 = insertvalue %Node %43, i64 %x32, 2
  ret %Node %44
}

define fastcc void
@"drop"(i64 %x92){
  %1 = inttoptr i64 %x92 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x91 = load i64, ptr %2
  switch i64 %x91, label %default_5 [i64 0, label %"0_5"]
"0_5":
  %3 = inttoptr i64 %x92 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 0
  %x90 = load i64, ptr %4
  switch i64 %x90, label %default_6 [i64 0, label %"Cnat_6"
                                     i64 1, label %"FAgda.Builtin.Nat._-__6"
                                     i64 3, label %"FDownFrom.downFrom_6"
                                     i64 5, label %"FDownFrom.sum_6"]
"Cnat_6":
  %5 = inttoptr i64 %x92 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 2
  %x85 = load i64, ptr %6
  %7 = inttoptr i64 %x92 to ptr
  call fastcc void @free(ptr %7)
  ret void
"FAgda.Builtin.Nat._-__6":
  %8 = inttoptr i64 %x92 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 2
  %x87 = load i64, ptr %9
  %10 = inttoptr i64 %x92 to ptr
  %11 = getelementptr inbounds %Node, ptr %10, i32 0, i64 3
  %x86 = load i64, ptr %11
  %12 = inttoptr i64 %x92 to ptr
  call fastcc void @free(ptr %12)
  ret void
"FDownFrom.downFrom_6":
  %13 = inttoptr i64 %x92 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 2
  %x88 = load i64, ptr %14
  %15 = inttoptr i64 %x92 to ptr
  call fastcc void @free(ptr %15)
  ret void
"FDownFrom.sum_6":
  %16 = inttoptr i64 %x92 to ptr
  %17 = getelementptr inbounds %Node, ptr %16, i32 0, i64 2
  %x89 = load i64, ptr %17
  %18 = inttoptr i64 %x92 to ptr
  call fastcc void @free(ptr %18)
  ret void
default_6:
  unreachable
default_5:
  %19 = inttoptr i64 %x91 to ptr
  %20 = getelementptr inbounds %Node, ptr %19, i32 0, i64 1
  %21 = load i64, ptr %20
  %22 = sub i64 %21, 1
  store i64 %22, ptr %19
  ret void
}