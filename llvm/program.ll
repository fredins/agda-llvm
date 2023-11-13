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
@"DownFrom.downFrom"(i64 %x7){
  %1 = inttoptr i64 %x7 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x56 = load i64, ptr %2
  switch i64 %x56, label %default_0 [i64 0, label %"Cnat_0"
                                     i64 1, label %"FAgda.Builtin.Nat._-__0"]
"Cnat_0":
  %3 = inttoptr i64 %x7 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 2
  %x71 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 1
  %"Cnat_0_res" = insertvalue %Node %5, i64 %x71, 2
  br label %continue_0
"FAgda.Builtin.Nat._-__0":
  %6 = inttoptr i64 %x7 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x72 = load i64, ptr %7
  %8 = inttoptr i64 %x7 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x73 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__0_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x72, i64 %x73)
  br label %continue_0
default_0:
  unreachable
continue_0:
  %10 = phi %Node  [%"Cnat_0_res", %"Cnat_0"]
                 , [%"FAgda.Builtin.Nat._-__0_res", %"FAgda.Builtin.Nat._-__0"]
  %x55 = extractvalue %Node %10, 2
  %11 = insertvalue %Node undef, i64 0, 1
  %12 = insertvalue %Node %11, i64 %x55, 2
  %13 = inttoptr i64 %x7 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 0
  %15 = load i64, ptr %14
  %16 = insertvalue %Node %12, i64 %15, 0
  store %Node %16, ptr %13
  switch i64 %x55, label %default_1 [i64 0, label %"0_1"]
"0_1":
  %17 = inttoptr i64 %x7 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 0
  %x84 = load i64, ptr %18
  switch i64 %x84, label %default_3 [i64 1, label %"1_3"]
"1_3":
  %19 = inttoptr i64 %x7 to ptr
  call fastcc void @free(ptr %19)
  br label %continue_2
default_3:
  %x83 = sub i64 %x84, 1
  %20 = inttoptr i64 %x7 to ptr
  %21 = getelementptr inbounds %Node, ptr %20, i32 0, i64 0
  store i64 %x83, ptr %21
  br label %continue_2
continue_2:
  %22 = insertvalue %Node undef, i64 2, 1
  ret %Node %22
default_1:
  %23 = insertvalue %Node undef, i64 0, 1
  %24 = insertvalue %Node %23, i64 1, 2
  %25 = insertvalue %Node %24, i64 1, 0
  %26 = call fastcc ptr @malloc(i64 32)
  store %Node %25, ptr %26
  %x5 = ptrtoint ptr %26 to i64
  %27 = insertvalue %Node undef, i64 1, 1
  %28 = insertvalue %Node %27, i64 %x7, 2
  %29 = insertvalue %Node %28, i64 %x5, 3
  %30 = insertvalue %Node %29, i64 1, 0
  %31 = call fastcc ptr @malloc(i64 32)
  store %Node %30, ptr %31
  %x4 = ptrtoint ptr %31 to i64
  call fastcc void @"dup"(i64 %x4)
  %32 = insertvalue %Node undef, i64 3, 1
  %33 = insertvalue %Node %32, i64 %x4, 2
  %34 = insertvalue %Node %33, i64 1, 0
  %35 = call fastcc ptr @malloc(i64 32)
  store %Node %34, ptr %35
  %x3 = ptrtoint ptr %35 to i64
  %36 = insertvalue %Node undef, i64 4, 1
  %37 = insertvalue %Node %36, i64 %x4, 2
  %38 = insertvalue %Node %37, i64 %x3, 3
  ret %Node %38
}

define fastcc %Node
@"DownFrom.sum"(i64 %x14){
  %1 = inttoptr i64 %x14 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 2
  %x40 = load i64, ptr %2
  %3 = call fastcc %Node @"DownFrom.downFrom"(i64 %x40)
  %x59 = extractvalue %Node %3, 1
  %x60 = extractvalue %Node %3, 2
  %x61 = extractvalue %Node %3, 3
  switch i64 %x59, label %default_4 [i64 2, label %"CDownFrom.List.[]_4"
                                     i64 4, label %"CDownFrom.List._∷__4"]
"CDownFrom.List.[]_4":
  %4 = insertvalue %Node undef, i64 2, 1
  %5 = inttoptr i64 %x14 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 0
  %7 = load i64, ptr %6
  %8 = insertvalue %Node %4, i64 %7, 0
  store %Node %8, ptr %5
  %9 = inttoptr i64 %x14 to ptr
  %10 = getelementptr inbounds %Node, ptr %9, i32 0, i64 0
  %x86 = load i64, ptr %10
  switch i64 %x86, label %default_6 [i64 1, label %"1_6"]
"1_6":
  %11 = inttoptr i64 %x14 to ptr
  call fastcc void @free(ptr %11)
  br label %continue_5
default_6:
  %x85 = sub i64 %x86, 1
  %12 = inttoptr i64 %x14 to ptr
  %13 = getelementptr inbounds %Node, ptr %12, i32 0, i64 0
  store i64 %x85, ptr %13
  br label %continue_5
continue_5:
  %14 = insertvalue %Node undef, i64 0, 1
  %15 = insertvalue %Node %14, i64 0, 2
  ret %Node %15
"CDownFrom.List._∷__4":
  %16 = insertvalue %Node undef, i64 4, 1
  %17 = insertvalue %Node %16, i64 %x60, 2
  %18 = insertvalue %Node %17, i64 %x61, 3
  %19 = inttoptr i64 %x14 to ptr
  %20 = getelementptr inbounds %Node, ptr %19, i32 0, i64 0
  %21 = load i64, ptr %20
  %22 = insertvalue %Node %18, i64 %21, 0
  store %Node %22, ptr %19
  %23 = inttoptr i64 %x14 to ptr
  %24 = getelementptr inbounds %Node, ptr %23, i32 0, i64 0
  %x88 = load i64, ptr %24
  switch i64 %x88, label %default_8 [i64 1, label %"1_8"]
"1_8":
  %25 = inttoptr i64 %x14 to ptr
  call fastcc void @free(ptr %25)
  br label %continue_7
default_8:
  call fastcc void @"dup"(i64 %x61)
  call fastcc void @"dup"(i64 %x60)
  %x87 = sub i64 %x88, 1
  %26 = inttoptr i64 %x14 to ptr
  %27 = getelementptr inbounds %Node, ptr %26, i32 0, i64 0
  store i64 %x87, ptr %27
  br label %continue_7
continue_7:
  %28 = insertvalue %Node undef, i64 5, 1
  %29 = insertvalue %Node %28, i64 %x61, 2
  %30 = insertvalue %Node %29, i64 1, 0
  %31 = call fastcc ptr @malloc(i64 32)
  store %Node %30, ptr %31
  %x10 = ptrtoint ptr %31 to i64
  %32 = musttail call
        fastcc
        %Node
        @"Agda.Builtin.Nat._+_"(i64 %x10, i64 %x60)
  ret %Node %32
default_4:
  unreachable
}

define fastcc void
@main(){
  %1 = insertvalue %Node undef, i64 0, 1
  %2 = insertvalue %Node %1, i64 100, 2
  %3 = insertvalue %Node %2, i64 1, 0
  %4 = call fastcc ptr @malloc(i64 32)
  store %Node %3, ptr %4
  %x20 = ptrtoint ptr %4 to i64
  %5 = insertvalue %Node undef, i64 3, 1
  %6 = insertvalue %Node %5, i64 %x20, 2
  %7 = insertvalue %Node %6, i64 1, 0
  %8 = call fastcc ptr @malloc(i64 32)
  store %Node %7, ptr %8
  %x19 = ptrtoint ptr %8 to i64
  %9 = call fastcc %Node @"DownFrom.sum"(i64 %x19)
  %x18 = extractvalue %Node %9, 2
  call fastcc void @printf(ptr @"%d", i64 %x18)
  ret void
}

define fastcc %Node
@"Agda.Builtin.Nat._+_"(i64 %x25, i64 %x26){
  %1 = inttoptr i64 %x25 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 2
  %x42 = load i64, ptr %2
  %3 = call fastcc %Node @"DownFrom.sum"(i64 %x42)
  %x66 = extractvalue %Node %3, 2
  %4 = insertvalue %Node undef, i64 0, 1
  %5 = insertvalue %Node %4, i64 %x66, 2
  %6 = inttoptr i64 %x25 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 0
  %8 = load i64, ptr %7
  %9 = insertvalue %Node %5, i64 %8, 0
  store %Node %9, ptr %6
  %10 = inttoptr i64 %x25 to ptr
  %11 = getelementptr inbounds %Node, ptr %10, i32 0, i64 0
  %x90 = load i64, ptr %11
  switch i64 %x90, label %default_10 [i64 1, label %"1_10"]
"1_10":
  %12 = inttoptr i64 %x25 to ptr
  call fastcc void @free(ptr %12)
  br label %continue_9
default_10:
  %x89 = sub i64 %x90, 1
  %13 = inttoptr i64 %x25 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 0
  store i64 %x89, ptr %14
  br label %continue_9
continue_9:
  %15 = inttoptr i64 %x26 to ptr
  %16 = getelementptr inbounds %Node, ptr %15, i32 0, i64 1
  %x63 = load i64, ptr %16
  switch i64 %x63, label %default_11 [i64 0, label %"Cnat_11"
                                      i64 1, label %"FAgda.Builtin.Nat._-__11"]
"Cnat_11":
  %17 = inttoptr i64 %x26 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 2
  %x74 = load i64, ptr %18
  %19 = insertvalue %Node undef, i64 0, 1
  %"Cnat_11_res" = insertvalue %Node %19, i64 %x74, 2
  br label %continue_11
"FAgda.Builtin.Nat._-__11":
  %20 = inttoptr i64 %x26 to ptr
  %21 = getelementptr inbounds %Node, ptr %20, i32 0, i64 2
  %x75 = load i64, ptr %21
  %22 = inttoptr i64 %x26 to ptr
  %23 = getelementptr inbounds %Node, ptr %22, i32 0, i64 3
  %x76 = load i64, ptr %23
  %"FAgda.Builtin.Nat._-__11_res" = call
                                    fastcc
                                    %Node
                                    @"Agda.Builtin.Nat._-_"(i64 %x75, i64 %x76)
  br label %continue_11
default_11:
  unreachable
continue_11:
  %24 = phi %Node  [%"Cnat_11_res", %"Cnat_11"]
                 , [%"FAgda.Builtin.Nat._-__11_res", %"FAgda.Builtin.Nat._-__11"]
  %x62 = extractvalue %Node %24, 2
  %25 = insertvalue %Node undef, i64 0, 1
  %26 = insertvalue %Node %25, i64 %x62, 2
  %27 = inttoptr i64 %x26 to ptr
  %28 = getelementptr inbounds %Node, ptr %27, i32 0, i64 0
  %29 = load i64, ptr %28
  %30 = insertvalue %Node %26, i64 %29, 0
  store %Node %30, ptr %27
  %31 = inttoptr i64 %x26 to ptr
  %32 = getelementptr inbounds %Node, ptr %31, i32 0, i64 0
  %x92 = load i64, ptr %32
  switch i64 %x92, label %default_13 [i64 1, label %"1_13"]
"1_13":
  %33 = inttoptr i64 %x26 to ptr
  call fastcc void @free(ptr %33)
  br label %continue_12
default_13:
  %x91 = sub i64 %x92, 1
  %34 = inttoptr i64 %x26 to ptr
  %35 = getelementptr inbounds %Node, ptr %34, i32 0, i64 0
  store i64 %x91, ptr %35
  br label %continue_12
continue_12:
  %x24 = add i64 %x66, %x62
  %36 = insertvalue %Node undef, i64 0, 1
  %37 = insertvalue %Node %36, i64 %x24, 2
  ret %Node %37
}

define fastcc %Node
@"Agda.Builtin.Nat._-_"(i64 %x31, i64 %x32){
  %1 = inttoptr i64 %x31 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x68 = load i64, ptr %2
  switch i64 %x68, label %default_14 [i64 0, label %"Cnat_14"
                                      i64 1, label %"FAgda.Builtin.Nat._-__14"]
"Cnat_14":
  %3 = inttoptr i64 %x31 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 2
  %x77 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 1
  %"Cnat_14_res" = insertvalue %Node %5, i64 %x77, 2
  br label %continue_14
"FAgda.Builtin.Nat._-__14":
  %6 = inttoptr i64 %x31 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x78 = load i64, ptr %7
  %8 = inttoptr i64 %x31 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x79 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__14_res" = call
                                    fastcc
                                    %Node
                                    @"Agda.Builtin.Nat._-_"(i64 %x78, i64 %x79)
  br label %continue_14
default_14:
  unreachable
continue_14:
  %10 = phi %Node  [%"Cnat_14_res", %"Cnat_14"]
                 , [%"FAgda.Builtin.Nat._-__14_res", %"FAgda.Builtin.Nat._-__14"]
  %x67 = extractvalue %Node %10, 2
  %11 = insertvalue %Node undef, i64 0, 1
  %12 = insertvalue %Node %11, i64 %x67, 2
  %13 = inttoptr i64 %x31 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 0
  %15 = load i64, ptr %14
  %16 = insertvalue %Node %12, i64 %15, 0
  store %Node %16, ptr %13
  %17 = inttoptr i64 %x31 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 0
  %x94 = load i64, ptr %18
  switch i64 %x94, label %default_16 [i64 1, label %"1_16"]
"1_16":
  %19 = inttoptr i64 %x31 to ptr
  call fastcc void @free(ptr %19)
  br label %continue_15
default_16:
  %x93 = sub i64 %x94, 1
  %20 = inttoptr i64 %x31 to ptr
  %21 = getelementptr inbounds %Node, ptr %20, i32 0, i64 0
  store i64 %x93, ptr %21
  br label %continue_15
continue_15:
  %22 = inttoptr i64 %x32 to ptr
  %23 = getelementptr inbounds %Node, ptr %22, i32 0, i64 2
  %x54 = load i64, ptr %23
  %24 = insertvalue %Node undef, i64 0, 1
  %25 = insertvalue %Node %24, i64 %x54, 2
  %26 = inttoptr i64 %x32 to ptr
  %27 = getelementptr inbounds %Node, ptr %26, i32 0, i64 0
  %28 = load i64, ptr %27
  %29 = insertvalue %Node %25, i64 %28, 0
  store %Node %29, ptr %26
  %30 = inttoptr i64 %x32 to ptr
  %31 = getelementptr inbounds %Node, ptr %30, i32 0, i64 0
  %x96 = load i64, ptr %31
  switch i64 %x96, label %default_18 [i64 1, label %"1_18"]
"1_18":
  %32 = inttoptr i64 %x32 to ptr
  call fastcc void @free(ptr %32)
  br label %continue_17
default_18:
  %x95 = sub i64 %x96, 1
  %33 = inttoptr i64 %x32 to ptr
  %34 = getelementptr inbounds %Node, ptr %33, i32 0, i64 0
  store i64 %x95, ptr %34
  br label %continue_17
continue_17:
  %x30 = sub i64 %x67, %x54
  %35 = insertvalue %Node undef, i64 0, 1
  %36 = insertvalue %Node %35, i64 %x30, 2
  ret %Node %36
}

define fastcc void
@"dup"(i64 %x82){
  %1 = inttoptr i64 %x82 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x81 = load i64, ptr %2
  %x80 = add i64 %x81, 1
  %3 = inttoptr i64 %x82 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 0
  store i64 %x80, ptr %4
  ret void
}