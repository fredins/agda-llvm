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
  %x164 = load i64, ptr %6
  %7 = inttoptr i64 %x8 to ptr
  %8 = getelementptr inbounds %Node, ptr %7, i32 0, i64 1
  %9 = load i64, ptr %8
  %10 = add i64 %9, 1
  store i64 %10, ptr %7
  %11 = inttoptr i64 %x8 to ptr
  %12 = getelementptr inbounds %Node, ptr %11, i32 0, i64 1
  %x165 = load i64, ptr %12
  switch i64 %x164, label %default_0 [i64 0, label %"Cnat_0"
                                      i64 1, label %"FAgda.Builtin.Nat._-__0"]
"Cnat_0":
  %13 = inttoptr i64 %x8 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 1
  %15 = load i64, ptr %14
  %16 = add i64 %15, 1
  store i64 %16, ptr %13
  %17 = inttoptr i64 %x8 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 2
  %x260 = load i64, ptr %18
  %19 = insertvalue %Node undef, i64 0, 0
  %20 = insertvalue %Node %19, i64 %x165, 1
  %"Cnat_0_res" = insertvalue %Node %20, i64 %x260, 2
  br label %continue_0
"FAgda.Builtin.Nat._-__0":
  %21 = inttoptr i64 %x8 to ptr
  %22 = getelementptr inbounds %Node, ptr %21, i32 0, i64 1
  %23 = load i64, ptr %22
  %24 = add i64 %23, 1
  store i64 %24, ptr %21
  %25 = inttoptr i64 %x8 to ptr
  %26 = getelementptr inbounds %Node, ptr %25, i32 0, i64 2
  %x261 = load i64, ptr %26
  %27 = inttoptr i64 %x8 to ptr
  %28 = getelementptr inbounds %Node, ptr %27, i32 0, i64 1
  %29 = load i64, ptr %28
  %30 = add i64 %29, 1
  store i64 %30, ptr %27
  %31 = inttoptr i64 %x8 to ptr
  %32 = getelementptr inbounds %Node, ptr %31, i32 0, i64 3
  %x262 = load i64, ptr %32
  %"FAgda.Builtin.Nat._-__0_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x261, i64 %x262)
  br label %continue_0
default_0:
  unreachable
continue_0:
  %33 = phi %Node  [%"Cnat_0_res", %"Cnat_0"]
                 , [%"FAgda.Builtin.Nat._-__0_res", %"FAgda.Builtin.Nat._-__0"]
  %x162 = extractvalue %Node %33, 1
  %x163 = extractvalue %Node %33, 2
  %34 = insertvalue %Node undef, i64 0, 0
  %35 = insertvalue %Node %34, i64 %x162, 1
  %36 = insertvalue %Node %35, i64 %x163, 2
  %37 = inttoptr i64 %x8 to ptr
  store %Node %36, ptr %37
  switch i64 %x163, label %default_1 [i64 0, label %"0_1"]
"0_1":
  %38 = insertvalue %Node undef, i64 2, 0
  %39 = insertvalue %Node %38, i64 1, 1
  ret %Node %39
default_1:
  %40 = insertvalue %Node undef, i64 0, 0
  %41 = insertvalue %Node %40, i64 1, 1
  %42 = insertvalue %Node %41, i64 1, 2
  %43 = call fastcc ptr @malloc(i64 192)
  store %Node %42, ptr %43
  %x2 = ptrtoint ptr %43 to i64
  %44 = insertvalue %Node undef, i64 1, 0
  %45 = insertvalue %Node %44, i64 1, 1
  %46 = insertvalue %Node %45, i64 %x8, 2
  %47 = insertvalue %Node %46, i64 %x2, 3
  %48 = call fastcc ptr @malloc(i64 192)
  store %Node %47, ptr %48
  %x5 = ptrtoint ptr %48 to i64
  %49 = insertvalue %Node undef, i64 3, 0
  %50 = insertvalue %Node %49, i64 1, 1
  %51 = insertvalue %Node %50, i64 %x5, 2
  %52 = call fastcc ptr @malloc(i64 192)
  store %Node %51, ptr %52
  %x4 = ptrtoint ptr %52 to i64
  %53 = insertvalue %Node undef, i64 4, 0
  %54 = insertvalue %Node %53, i64 1, 1
  %55 = insertvalue %Node %54, i64 %x5, 2
  %56 = insertvalue %Node %55, i64 %x4, 3
  ret %Node %56
}

define fastcc %Node
@"DownFrom.sum"(i64 %x17){
  %1 = inttoptr i64 %x17 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %3 = load i64, ptr %2
  %4 = add i64 %3, 1
  store i64 %4, ptr %1
  %5 = inttoptr i64 %x17 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 1
  %x50 = load i64, ptr %6
  %7 = inttoptr i64 %x17 to ptr
  %8 = getelementptr inbounds %Node, ptr %7, i32 0, i64 1
  %9 = load i64, ptr %8
  %10 = add i64 %9, 1
  store i64 %10, ptr %7
  %11 = inttoptr i64 %x17 to ptr
  %12 = getelementptr inbounds %Node, ptr %11, i32 0, i64 2
  %x51 = load i64, ptr %12
  %13 = call fastcc %Node @"DownFrom.downFrom"(i64 %x51)
  %x168 = extractvalue %Node %13, 0
  %x169 = extractvalue %Node %13, 1
  %x170 = extractvalue %Node %13, 2
  %x171 = extractvalue %Node %13, 3
  switch i64 %x168, label %default_2 [i64 2, label %"CDownFrom.List.[]_2"
                                      i64 4, label %"CDownFrom.List._∷__2"]
"CDownFrom.List.[]_2":
  %14 = inttoptr i64 %x170 to ptr
  %15 = getelementptr inbounds %Node, ptr %14, i32 0, i64 1
  %16 = load i64, ptr %15
  %17 = add i64 %16, 1
  store i64 %17, ptr %14
  %18 = inttoptr i64 %x171 to ptr
  %19 = getelementptr inbounds %Node, ptr %18, i32 0, i64 1
  %20 = load i64, ptr %19
  %21 = add i64 %20, 1
  store i64 %21, ptr %18
  %22 = insertvalue %Node undef, i64 %x168, 0
  %23 = insertvalue %Node %22, i64 %x169, 1
  %24 = insertvalue %Node %23, i64 %x170, 2
  %25 = insertvalue %Node %24, i64 %x171, 3
  %26 = inttoptr i64 %x17 to ptr
  store %Node %25, ptr %26
  %27 = insertvalue %Node undef, i64 0, 0
  %28 = insertvalue %Node %27, i64 1, 1
  %29 = insertvalue %Node %28, i64 0, 2
  ret %Node %29
"CDownFrom.List._∷__2":
  %30 = inttoptr i64 %x170 to ptr
  %31 = getelementptr inbounds %Node, ptr %30, i32 0, i64 1
  %32 = load i64, ptr %31
  %33 = add i64 %32, 1
  store i64 %33, ptr %30
  %34 = inttoptr i64 %x171 to ptr
  %35 = getelementptr inbounds %Node, ptr %34, i32 0, i64 1
  %36 = load i64, ptr %35
  %37 = add i64 %36, 1
  store i64 %37, ptr %34
  %38 = insertvalue %Node undef, i64 %x168, 0
  %39 = insertvalue %Node %38, i64 %x169, 1
  %40 = insertvalue %Node %39, i64 %x170, 2
  %41 = insertvalue %Node %40, i64 %x171, 3
  %42 = inttoptr i64 %x17 to ptr
  store %Node %41, ptr %42
  %43 = insertvalue %Node undef, i64 5, 0
  %44 = insertvalue %Node %43, i64 1, 1
  %45 = insertvalue %Node %44, i64 %x171, 2
  %46 = call fastcc ptr @malloc(i64 192)
  store %Node %45, ptr %46
  %x12 = ptrtoint ptr %46 to i64
  %47 = call
        fastcc
        %Node
        @"Agda.Builtin.Nat._+_"(i64 %x12, i64 %x170)
  ret %Node %47
default_2:
  unreachable
}

define fastcc void
@main(){
  %1 = insertvalue %Node undef, i64 0, 0
  %2 = insertvalue %Node %1, i64 1, 1
  %3 = insertvalue %Node %2, i64 4, 2
  %4 = call fastcc ptr @malloc(i64 192)
  store %Node %3, ptr %4
  %x21 = ptrtoint ptr %4 to i64
  %5 = insertvalue %Node undef, i64 3, 0
  %6 = insertvalue %Node %5, i64 1, 1
  %7 = insertvalue %Node %6, i64 %x21, 2
  %8 = call fastcc ptr @malloc(i64 192)
  store %Node %7, ptr %8
  %x24 = ptrtoint ptr %8 to i64
  %9 = call fastcc %Node @"DownFrom.sum"(i64 %x24)
  %x22 = extractvalue %Node %9, 1
  %x23 = extractvalue %Node %9, 2
  call fastcc void @printf(ptr @"%d", i64 %x23)
  ret void
}

define fastcc %Node
@"Agda.Builtin.Nat._+_"(i64 %x31, i64 %x32){
  %1 = inttoptr i64 %x31 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %3 = load i64, ptr %2
  %4 = add i64 %3, 1
  store i64 %4, ptr %1
  %5 = inttoptr i64 %x31 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 1
  %x53 = load i64, ptr %6
  %7 = inttoptr i64 %x31 to ptr
  %8 = getelementptr inbounds %Node, ptr %7, i32 0, i64 1
  %9 = load i64, ptr %8
  %10 = add i64 %9, 1
  store i64 %10, ptr %7
  %11 = inttoptr i64 %x31 to ptr
  %12 = getelementptr inbounds %Node, ptr %11, i32 0, i64 2
  %x54 = load i64, ptr %12
  %13 = call fastcc %Node @"DownFrom.sum"(i64 %x54)
  %x178 = extractvalue %Node %13, 1
  %x179 = extractvalue %Node %13, 2
  %14 = insertvalue %Node undef, i64 0, 0
  %15 = insertvalue %Node %14, i64 %x178, 1
  %16 = insertvalue %Node %15, i64 %x179, 2
  %17 = inttoptr i64 %x31 to ptr
  store %Node %16, ptr %17
  %18 = inttoptr i64 %x32 to ptr
  %19 = getelementptr inbounds %Node, ptr %18, i32 0, i64 1
  %20 = load i64, ptr %19
  %21 = add i64 %20, 1
  store i64 %21, ptr %18
  %22 = inttoptr i64 %x32 to ptr
  %23 = getelementptr inbounds %Node, ptr %22, i32 0, i64 0
  %x174 = load i64, ptr %23
  %24 = inttoptr i64 %x32 to ptr
  %25 = getelementptr inbounds %Node, ptr %24, i32 0, i64 1
  %26 = load i64, ptr %25
  %27 = add i64 %26, 1
  store i64 %27, ptr %24
  %28 = inttoptr i64 %x32 to ptr
  %29 = getelementptr inbounds %Node, ptr %28, i32 0, i64 1
  %x175 = load i64, ptr %29
  switch i64 %x174, label %default_3 [i64 0, label %"Cnat_3"
                                      i64 1, label %"FAgda.Builtin.Nat._-__3"]
"Cnat_3":
  %30 = inttoptr i64 %x32 to ptr
  %31 = getelementptr inbounds %Node, ptr %30, i32 0, i64 1
  %32 = load i64, ptr %31
  %33 = add i64 %32, 1
  store i64 %33, ptr %30
  %34 = inttoptr i64 %x32 to ptr
  %35 = getelementptr inbounds %Node, ptr %34, i32 0, i64 2
  %x263 = load i64, ptr %35
  %36 = insertvalue %Node undef, i64 0, 0
  %37 = insertvalue %Node %36, i64 %x175, 1
  %"Cnat_3_res" = insertvalue %Node %37, i64 %x263, 2
  br label %continue_3
"FAgda.Builtin.Nat._-__3":
  %38 = inttoptr i64 %x32 to ptr
  %39 = getelementptr inbounds %Node, ptr %38, i32 0, i64 1
  %40 = load i64, ptr %39
  %41 = add i64 %40, 1
  store i64 %41, ptr %38
  %42 = inttoptr i64 %x32 to ptr
  %43 = getelementptr inbounds %Node, ptr %42, i32 0, i64 2
  %x264 = load i64, ptr %43
  %44 = inttoptr i64 %x32 to ptr
  %45 = getelementptr inbounds %Node, ptr %44, i32 0, i64 1
  %46 = load i64, ptr %45
  %47 = add i64 %46, 1
  store i64 %47, ptr %44
  %48 = inttoptr i64 %x32 to ptr
  %49 = getelementptr inbounds %Node, ptr %48, i32 0, i64 3
  %x265 = load i64, ptr %49
  %"FAgda.Builtin.Nat._-__3_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x264, i64 %x265)
  br label %continue_3
default_3:
  unreachable
continue_3:
  %50 = phi %Node  [%"Cnat_3_res", %"Cnat_3"]
                 , [%"FAgda.Builtin.Nat._-__3_res", %"FAgda.Builtin.Nat._-__3"]
  %x172 = extractvalue %Node %50, 1
  %x173 = extractvalue %Node %50, 2
  %51 = insertvalue %Node undef, i64 0, 0
  %52 = insertvalue %Node %51, i64 %x172, 1
  %53 = insertvalue %Node %52, i64 %x173, 2
  %54 = inttoptr i64 %x32 to ptr
  store %Node %53, ptr %54
  %x30 = add i64 %x179, %x173
  %55 = insertvalue %Node undef, i64 0, 0
  %56 = insertvalue %Node %55, i64 1, 1
  %57 = insertvalue %Node %56, i64 %x30, 2
  ret %Node %57
}

define fastcc %Node
@"Agda.Builtin.Nat._-_"(i64 %x39, i64 %x40){
  %1 = inttoptr i64 %x39 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %3 = load i64, ptr %2
  %4 = add i64 %3, 1
  store i64 %4, ptr %1
  %5 = inttoptr i64 %x39 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 0
  %x184 = load i64, ptr %6
  %7 = inttoptr i64 %x39 to ptr
  %8 = getelementptr inbounds %Node, ptr %7, i32 0, i64 1
  %9 = load i64, ptr %8
  %10 = add i64 %9, 1
  store i64 %10, ptr %7
  %11 = inttoptr i64 %x39 to ptr
  %12 = getelementptr inbounds %Node, ptr %11, i32 0, i64 1
  %x185 = load i64, ptr %12
  switch i64 %x184, label %default_4 [i64 0, label %"Cnat_4"
                                      i64 1, label %"FAgda.Builtin.Nat._-__4"]
"Cnat_4":
  %13 = inttoptr i64 %x39 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 1
  %15 = load i64, ptr %14
  %16 = add i64 %15, 1
  store i64 %16, ptr %13
  %17 = inttoptr i64 %x39 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 2
  %x266 = load i64, ptr %18
  %19 = insertvalue %Node undef, i64 0, 0
  %20 = insertvalue %Node %19, i64 %x185, 1
  %"Cnat_4_res" = insertvalue %Node %20, i64 %x266, 2
  br label %continue_4
"FAgda.Builtin.Nat._-__4":
  %21 = inttoptr i64 %x39 to ptr
  %22 = getelementptr inbounds %Node, ptr %21, i32 0, i64 1
  %23 = load i64, ptr %22
  %24 = add i64 %23, 1
  store i64 %24, ptr %21
  %25 = inttoptr i64 %x39 to ptr
  %26 = getelementptr inbounds %Node, ptr %25, i32 0, i64 2
  %x267 = load i64, ptr %26
  %27 = inttoptr i64 %x39 to ptr
  %28 = getelementptr inbounds %Node, ptr %27, i32 0, i64 1
  %29 = load i64, ptr %28
  %30 = add i64 %29, 1
  store i64 %30, ptr %27
  %31 = inttoptr i64 %x39 to ptr
  %32 = getelementptr inbounds %Node, ptr %31, i32 0, i64 3
  %x268 = load i64, ptr %32
  %"FAgda.Builtin.Nat._-__4_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x267, i64 %x268)
  br label %continue_4
default_4:
  unreachable
continue_4:
  %33 = phi %Node  [%"Cnat_4_res", %"Cnat_4"]
                 , [%"FAgda.Builtin.Nat._-__4_res", %"FAgda.Builtin.Nat._-__4"]
  %x182 = extractvalue %Node %33, 1
  %x183 = extractvalue %Node %33, 2
  %34 = insertvalue %Node undef, i64 0, 0
  %35 = insertvalue %Node %34, i64 %x182, 1
  %36 = insertvalue %Node %35, i64 %x183, 2
  %37 = inttoptr i64 %x39 to ptr
  store %Node %36, ptr %37
  %38 = inttoptr i64 %x40 to ptr
  %39 = getelementptr inbounds %Node, ptr %38, i32 0, i64 1
  %40 = load i64, ptr %39
  %41 = add i64 %40, 1
  store i64 %41, ptr %38
  %42 = inttoptr i64 %x40 to ptr
  %43 = getelementptr inbounds %Node, ptr %42, i32 0, i64 1
  %x70 = load i64, ptr %43
  %44 = inttoptr i64 %x40 to ptr
  %45 = getelementptr inbounds %Node, ptr %44, i32 0, i64 1
  %46 = load i64, ptr %45
  %47 = add i64 %46, 1
  store i64 %47, ptr %44
  %48 = inttoptr i64 %x40 to ptr
  %49 = getelementptr inbounds %Node, ptr %48, i32 0, i64 2
  %x71 = load i64, ptr %49
  %50 = insertvalue %Node undef, i64 0, 0
  %51 = insertvalue %Node %50, i64 %x70, 1
  %52 = insertvalue %Node %51, i64 %x71, 2
  %53 = inttoptr i64 %x40 to ptr
  store %Node %52, ptr %53
  %x38 = sub i64 %x183, %x71
  %54 = insertvalue %Node undef, i64 0, 0
  %55 = insertvalue %Node %54, i64 1, 1
  %56 = insertvalue %Node %55, i64 %x38, 2
  ret %Node %56
}

define fastcc void
@"drop"(i64 %x293){
  %1 = inttoptr i64 %x293 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x292 = load i64, ptr %2
  switch i64 %x292, label %default_5 [i64 1, label %"1_5"]
"1_5":
  %3 = inttoptr i64 %x293 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 0
  %x291 = load i64, ptr %4
  switch i64 %x291, label %default_6 [i64 0, label %"Cnat_6"
                                      i64 1, label %"FAgda.Builtin.Nat._-__6"
                                      i64 3, label %"FDownFrom.downFrom_6"
                                      i64 5, label %"FDownFrom.sum_6"]
"Cnat_6":
  %5 = inttoptr i64 %x293 to ptr
  call fastcc void @free(ptr %5)
  ret void
"FAgda.Builtin.Nat._-__6":
  %6 = inttoptr i64 %x293 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x288 = load i64, ptr %7
  %8 = inttoptr i64 %x293 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x287 = load i64, ptr %9
  %10 = inttoptr i64 %x293 to ptr
  call fastcc void @free(ptr %10)
  ret void
"FDownFrom.downFrom_6":
  %11 = inttoptr i64 %x293 to ptr
  %12 = getelementptr inbounds %Node, ptr %11, i32 0, i64 2
  %x289 = load i64, ptr %12
  %13 = inttoptr i64 %x293 to ptr
  call fastcc void @free(ptr %13)
  ret void
"FDownFrom.sum_6":
  %14 = inttoptr i64 %x293 to ptr
  %15 = getelementptr inbounds %Node, ptr %14, i32 0, i64 2
  %x290 = load i64, ptr %15
  %16 = inttoptr i64 %x293 to ptr
  call fastcc void @free(ptr %16)
  ret void
default_6:
  unreachable
default_5:
  %17 = inttoptr i64 %x292 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 1
  %19 = load i64, ptr %18
  %20 = sub i64 %19, 1
  store i64 %20, ptr %17
  ret void
}