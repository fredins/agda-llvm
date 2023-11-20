target triple = "x86_64-unknown-linux-gnu"
declare void @printf(ptr, ...)
declare ptr @malloc(i64)
declare void @free(ptr)
%Node = type [4 x i64]
@"%d" = private constant [4 x i8] c"%d\0A\00", align 1

; Tag numbering table:
; 0 Cnat
; 1 FAgda.Builtin.Nat._-_
; 2 CDownFromOpt.List.[]
; 3 FDownFromOpt.downFrom
; 4 CDownFromOpt.List._∷_

define fastcc %Node
@"Agda.Builtin.Nat._+_"(i64 %x3, i64 %x4){
  %1 = inttoptr i64 %x3 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x65 = load i64, ptr %2
  switch i64 %x65, label %default_0 [i64 0, label %"Cnat_0"
                                     i64 1, label %"FAgda.Builtin.Nat._-__0"]
"Cnat_0":
  %3 = inttoptr i64 %x3 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 2
  %x82 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 1
  %"Cnat_0_res" = insertvalue %Node %5, i64 %x82, 2
  br label %continue_0
"FAgda.Builtin.Nat._-__0":
  %6 = inttoptr i64 %x3 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x83 = load i64, ptr %7
  %8 = inttoptr i64 %x3 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x84 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__0_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x83, i64 %x84)
  br label %continue_0
default_0:
  unreachable
continue_0:
  %10 = phi %Node  [%"Cnat_0_res", %"Cnat_0"]
                 , [%"FAgda.Builtin.Nat._-__0_res", %"FAgda.Builtin.Nat._-__0"]
  %x64 = extractvalue %Node %10, 2
  %11 = insertvalue %Node undef, i64 0, 1
  %12 = insertvalue %Node %11, i64 %x64, 2
  %13 = inttoptr i64 %x3 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 0
  %15 = load i64, ptr %14
  %16 = insertvalue %Node %12, i64 %15, 0
  store %Node %16, ptr %13
  %17 = inttoptr i64 %x3 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 0
  %x107 = load i64, ptr %18
  switch i64 %x107, label %default_2 [i64 1, label %"1_2"]
"1_2":
  %19 = inttoptr i64 %x3 to ptr
  call fastcc void @free(ptr %19)
  br label %continue_1
default_2:
  %x106 = sub i64 %x107, 1
  %20 = inttoptr i64 %x3 to ptr
  %21 = getelementptr inbounds %Node, ptr %20, i32 0, i64 0
  store i64 %x106, ptr %21
  br label %continue_1
continue_1:
  %22 = inttoptr i64 %x4 to ptr
  %23 = getelementptr inbounds %Node, ptr %22, i32 0, i64 2
  %x44 = load i64, ptr %23
  %24 = insertvalue %Node undef, i64 0, 1
  %25 = insertvalue %Node %24, i64 %x44, 2
  %26 = inttoptr i64 %x4 to ptr
  %27 = getelementptr inbounds %Node, ptr %26, i32 0, i64 0
  %28 = load i64, ptr %27
  %29 = insertvalue %Node %25, i64 %28, 0
  store %Node %29, ptr %26
  %30 = inttoptr i64 %x4 to ptr
  %31 = getelementptr inbounds %Node, ptr %30, i32 0, i64 0
  %x109 = load i64, ptr %31
  switch i64 %x109, label %default_4 [i64 1, label %"1_4"]
"1_4":
  %32 = inttoptr i64 %x4 to ptr
  call fastcc void @free(ptr %32)
  br label %continue_3
default_4:
  %x108 = sub i64 %x109, 1
  %33 = inttoptr i64 %x4 to ptr
  %34 = getelementptr inbounds %Node, ptr %33, i32 0, i64 0
  store i64 %x108, ptr %34
  br label %continue_3
continue_3:
  %x2 = add i64 %x64, %x44
  %35 = insertvalue %Node undef, i64 0, 1
  %36 = insertvalue %Node %35, i64 %x2, 2
  ret %Node %36
}

define fastcc %Node
@"Agda.Builtin.Nat._-_"(i64 %x9, i64 %x10){
  %1 = inttoptr i64 %x9 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x69 = load i64, ptr %2
  switch i64 %x69, label %default_5 [i64 0, label %"Cnat_5"
                                     i64 1, label %"FAgda.Builtin.Nat._-__5"]
"Cnat_5":
  %3 = inttoptr i64 %x9 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 2
  %x85 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 1
  %"Cnat_5_res" = insertvalue %Node %5, i64 %x85, 2
  br label %continue_5
"FAgda.Builtin.Nat._-__5":
  %6 = inttoptr i64 %x9 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x86 = load i64, ptr %7
  %8 = inttoptr i64 %x9 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x87 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__5_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x86, i64 %x87)
  br label %continue_5
default_5:
  unreachable
continue_5:
  %10 = phi %Node  [%"Cnat_5_res", %"Cnat_5"]
                 , [%"FAgda.Builtin.Nat._-__5_res", %"FAgda.Builtin.Nat._-__5"]
  %x68 = extractvalue %Node %10, 2
  %11 = insertvalue %Node undef, i64 0, 1
  %12 = insertvalue %Node %11, i64 %x68, 2
  %13 = inttoptr i64 %x9 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 0
  %15 = load i64, ptr %14
  %16 = insertvalue %Node %12, i64 %15, 0
  store %Node %16, ptr %13
  %17 = inttoptr i64 %x9 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 0
  %x111 = load i64, ptr %18
  switch i64 %x111, label %default_7 [i64 1, label %"1_7"]
"1_7":
  %19 = inttoptr i64 %x9 to ptr
  call fastcc void @free(ptr %19)
  br label %continue_6
default_7:
  %x110 = sub i64 %x111, 1
  %20 = inttoptr i64 %x9 to ptr
  %21 = getelementptr inbounds %Node, ptr %20, i32 0, i64 0
  store i64 %x110, ptr %21
  br label %continue_6
continue_6:
  %22 = inttoptr i64 %x10 to ptr
  %23 = getelementptr inbounds %Node, ptr %22, i32 0, i64 2
  %x51 = load i64, ptr %23
  %24 = insertvalue %Node undef, i64 0, 1
  %25 = insertvalue %Node %24, i64 %x51, 2
  %26 = inttoptr i64 %x10 to ptr
  %27 = getelementptr inbounds %Node, ptr %26, i32 0, i64 0
  %28 = load i64, ptr %27
  %29 = insertvalue %Node %25, i64 %28, 0
  store %Node %29, ptr %26
  %30 = inttoptr i64 %x10 to ptr
  %31 = getelementptr inbounds %Node, ptr %30, i32 0, i64 0
  %x113 = load i64, ptr %31
  switch i64 %x113, label %default_9 [i64 1, label %"1_9"]
"1_9":
  %32 = inttoptr i64 %x10 to ptr
  call fastcc void @free(ptr %32)
  br label %continue_8
default_9:
  %x112 = sub i64 %x113, 1
  %33 = inttoptr i64 %x10 to ptr
  %34 = getelementptr inbounds %Node, ptr %33, i32 0, i64 0
  store i64 %x112, ptr %34
  br label %continue_8
continue_8:
  %x8 = sub i64 %x68, %x51
  %35 = insertvalue %Node undef, i64 0, 1
  %36 = insertvalue %Node %35, i64 %x8, 2
  ret %Node %36
}

define fastcc %Node
@"DownFromOpt.downFrom"(i64 %x19){
  %1 = inttoptr i64 %x19 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x73 = load i64, ptr %2
  switch i64 %x73, label %default_10 [i64 0, label %"Cnat_10"
                                      i64 1, label %"FAgda.Builtin.Nat._-__10"]
"Cnat_10":
  %3 = inttoptr i64 %x19 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 2
  %x88 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 1
  %"Cnat_10_res" = insertvalue %Node %5, i64 %x88, 2
  br label %continue_10
"FAgda.Builtin.Nat._-__10":
  %6 = inttoptr i64 %x19 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x89 = load i64, ptr %7
  %8 = inttoptr i64 %x19 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x90 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__10_res" = call
                                    fastcc
                                    %Node
                                    @"Agda.Builtin.Nat._-_"(i64 %x89, i64 %x90)
  br label %continue_10
default_10:
  unreachable
continue_10:
  %10 = phi %Node  [%"Cnat_10_res", %"Cnat_10"]
                 , [%"FAgda.Builtin.Nat._-__10_res", %"FAgda.Builtin.Nat._-__10"]
  %x72 = extractvalue %Node %10, 2
  %11 = insertvalue %Node undef, i64 0, 1
  %12 = insertvalue %Node %11, i64 %x72, 2
  %13 = inttoptr i64 %x19 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 0
  %15 = load i64, ptr %14
  %16 = insertvalue %Node %12, i64 %15, 0
  store %Node %16, ptr %13
  switch i64 %x72, label %default_11 [i64 0, label %"0_11"]
"0_11":
  %17 = inttoptr i64 %x19 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 0
  %x115 = load i64, ptr %18
  switch i64 %x115, label %default_13 [i64 1, label %"1_13"]
"1_13":
  %19 = inttoptr i64 %x19 to ptr
  call fastcc void @free(ptr %19)
  br label %continue_12
default_13:
  %x114 = sub i64 %x115, 1
  %20 = inttoptr i64 %x19 to ptr
  %21 = getelementptr inbounds %Node, ptr %20, i32 0, i64 0
  store i64 %x114, ptr %21
  br label %continue_12
continue_12:
  %22 = insertvalue %Node undef, i64 2, 1
  ret %Node %22
default_11:
  %23 = insertvalue %Node undef, i64 0, 1
  %24 = insertvalue %Node %23, i64 1, 2
  %25 = insertvalue %Node %24, i64 1, 0
  %26 = call fastcc ptr @malloc(i64 32)
  store %Node %25, ptr %26
  %x17 = ptrtoint ptr %26 to i64
  %27 = insertvalue %Node undef, i64 1, 1
  %28 = insertvalue %Node %27, i64 %x19, 2
  %29 = insertvalue %Node %28, i64 %x17, 3
  %30 = insertvalue %Node %29, i64 1, 0
  %31 = call fastcc ptr @malloc(i64 32)
  store %Node %30, ptr %31
  %x16 = ptrtoint ptr %31 to i64
  call fastcc void @"dup"(i64 %x16)
  %32 = insertvalue %Node undef, i64 3, 1
  %33 = insertvalue %Node %32, i64 %x16, 2
  %34 = insertvalue %Node %33, i64 1, 0
  %35 = call fastcc ptr @malloc(i64 32)
  store %Node %34, ptr %35
  %x15 = ptrtoint ptr %35 to i64
  %36 = insertvalue %Node undef, i64 4, 1
  %37 = insertvalue %Node %36, i64 %x16, 2
  %38 = insertvalue %Node %37, i64 %x15, 3
  ret %Node %38
}

define fastcc %Node
@"DownFromOpt.sum"(i64 %x27, i64 %x28){
  %1 = inttoptr i64 %x28 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x79 = load i64, ptr %2
  switch i64 %x79, label %default_14 [i64 2, label %"CDownFromOpt.List.[]_14"
                                      i64 4, label %"CDownFromOpt.List._∷__14"
                                      i64 3, label %"FDownFromOpt.downFrom_14"]
"CDownFromOpt.List.[]_14":
  %"CDownFromOpt.List.[]_14_res" = insertvalue %Node undef, i64 2, 1
  br label %continue_14
"CDownFromOpt.List._∷__14":
  %3 = inttoptr i64 %x28 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 2
  %x91 = load i64, ptr %4
  %5 = inttoptr i64 %x28 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 3
  %x93 = load i64, ptr %6
  %7 = insertvalue %Node undef, i64 4, 1
  %8 = insertvalue %Node %7, i64 %x91, 2
  %"CDownFromOpt.List._∷__14_res" = insertvalue %Node %8, i64 %x93, 3
  br label %continue_14
"FDownFromOpt.downFrom_14":
  %9 = inttoptr i64 %x28 to ptr
  %10 = getelementptr inbounds %Node, ptr %9, i32 0, i64 2
  %x92 = load i64, ptr %10
  %"FDownFromOpt.downFrom_14_res" = call
                                    fastcc
                                    %Node
                                    @"DownFromOpt.downFrom"(i64 %x92)
  br label %continue_14
default_14:
  unreachable
continue_14:
  %11 = phi %Node  [%"CDownFromOpt.List.[]_14_res", %"CDownFromOpt.List.[]_14"]
                 , [%"CDownFromOpt.List._∷__14_res", %"CDownFromOpt.List._∷__14"]
                 , [%"FDownFromOpt.downFrom_14_res", %"FDownFromOpt.downFrom_14"]
  %x76 = extractvalue %Node %11, 1
  %x77 = extractvalue %Node %11, 2
  %x78 = extractvalue %Node %11, 3
  switch i64 %x76, label %default_15 [i64 2, label %"CDownFromOpt.List.[]_15"
                                      i64 4, label %"CDownFromOpt.List._∷__15"]
"CDownFromOpt.List.[]_15":
  %12 = insertvalue %Node undef, i64 2, 1
  %13 = inttoptr i64 %x28 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 0
  %15 = load i64, ptr %14
  %16 = insertvalue %Node %12, i64 %15, 0
  store %Node %16, ptr %13
  %17 = inttoptr i64 %x28 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 0
  %x117 = load i64, ptr %18
  switch i64 %x117, label %default_17 [i64 1, label %"1_17"]
"1_17":
  %19 = inttoptr i64 %x28 to ptr
  call fastcc void @free(ptr %19)
  br label %continue_16
default_17:
  %x116 = sub i64 %x117, 1
  %20 = inttoptr i64 %x28 to ptr
  %21 = getelementptr inbounds %Node, ptr %20, i32 0, i64 0
  store i64 %x116, ptr %21
  br label %continue_16
continue_16:
  %22 = inttoptr i64 %x27 to ptr
  %23 = getelementptr inbounds %Node, ptr %22, i32 0, i64 2
  %x63 = load i64, ptr %23
  %24 = insertvalue %Node undef, i64 0, 1
  %25 = insertvalue %Node %24, i64 %x63, 2
  %26 = inttoptr i64 %x27 to ptr
  %27 = getelementptr inbounds %Node, ptr %26, i32 0, i64 0
  %28 = load i64, ptr %27
  %29 = insertvalue %Node %25, i64 %28, 0
  store %Node %29, ptr %26
  %30 = inttoptr i64 %x27 to ptr
  %31 = getelementptr inbounds %Node, ptr %30, i32 0, i64 0
  %x119 = load i64, ptr %31
  switch i64 %x119, label %default_19 [i64 1, label %"1_19"]
"1_19":
  %32 = inttoptr i64 %x27 to ptr
  call fastcc void @free(ptr %32)
  br label %continue_18
default_19:
  %x118 = sub i64 %x119, 1
  %33 = inttoptr i64 %x27 to ptr
  %34 = getelementptr inbounds %Node, ptr %33, i32 0, i64 0
  store i64 %x118, ptr %34
  br label %continue_18
continue_18:
  %35 = insertvalue %Node undef, i64 0, 1
  %36 = insertvalue %Node %35, i64 %x63, 2
  ret %Node %36
"CDownFromOpt.List._∷__15":
  %37 = insertvalue %Node undef, i64 4, 1
  %38 = insertvalue %Node %37, i64 %x77, 2
  %39 = insertvalue %Node %38, i64 %x78, 3
  %40 = inttoptr i64 %x28 to ptr
  %41 = getelementptr inbounds %Node, ptr %40, i32 0, i64 0
  %42 = load i64, ptr %41
  %43 = insertvalue %Node %39, i64 %42, 0
  store %Node %43, ptr %40
  %44 = inttoptr i64 %x28 to ptr
  %45 = getelementptr inbounds %Node, ptr %44, i32 0, i64 0
  %x121 = load i64, ptr %45
  switch i64 %x121, label %default_21 [i64 1, label %"1_21"]
"1_21":
  %46 = inttoptr i64 %x28 to ptr
  call fastcc void @free(ptr %46)
  br label %continue_20
default_21:
  call fastcc void @"dup"(i64 %x78)
  call fastcc void @"dup"(i64 %x77)
  %x120 = sub i64 %x121, 1
  %47 = inttoptr i64 %x28 to ptr
  %48 = getelementptr inbounds %Node, ptr %47, i32 0, i64 0
  store i64 %x120, ptr %48
  br label %continue_20
continue_20:
  %49 = call fastcc %Node @"Agda.Builtin.Nat._+_"(i64 %x77, i64 %x27)
  %x23 = extractvalue %Node %49, 2
  %50 = insertvalue %Node undef, i64 0, 1
  %51 = insertvalue %Node %50, i64 %x23, 2
  %52 = insertvalue %Node %51, i64 1, 0
  %53 = call fastcc ptr @malloc(i64 32)
  store %Node %52, ptr %53
  %x22 = ptrtoint ptr %53 to i64
  %54 = musttail call
        fastcc
        %Node
        @"DownFromOpt.sum"(i64 %x22, i64 %x78)
  ret %Node %54
default_15:
  unreachable
}

define fastcc void
@main(){
  %1 = insertvalue %Node undef, i64 0, 1
  %2 = insertvalue %Node %1, i64 0, 2
  %3 = insertvalue %Node %2, i64 1, 0
  %4 = call fastcc ptr @malloc(i64 32)
  store %Node %3, ptr %4
  %x36 = ptrtoint ptr %4 to i64
  %5 = insertvalue %Node undef, i64 0, 1
  %6 = insertvalue %Node %5, i64 100, 2
  %7 = insertvalue %Node %6, i64 1, 0
  %8 = call fastcc ptr @malloc(i64 32)
  store %Node %7, ptr %8
  %x35 = ptrtoint ptr %8 to i64
  %9 = insertvalue %Node undef, i64 3, 1
  %10 = insertvalue %Node %9, i64 %x35, 2
  %11 = insertvalue %Node %10, i64 1, 0
  %12 = call fastcc ptr @malloc(i64 32)
  store %Node %11, ptr %12
  %x34 = ptrtoint ptr %12 to i64
  %13 = call fastcc %Node @"DownFromOpt.sum"(i64 %x36, i64 %x34)
  %x33 = extractvalue %Node %13, 2
  call fastcc void @printf(ptr @"%d", i64 %x33)
  ret void
}

define fastcc void
@"drop"(i64 %x102){
  %1 = inttoptr i64 %x102 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x101 = load i64, ptr %2
  switch i64 %x101, label %default_22 [i64 1, label %"1_22"]
"1_22":
  %3 = inttoptr i64 %x102 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 1
  %x100 = load i64, ptr %4
  switch i64 %x100, label %default_23 [i64 2, label %"CDownFromOpt.List.[]_23"
                                       i64 4, label %"CDownFromOpt.List._∷__23"
                                       i64 0, label %"Cnat_23"
                                       i64 1, label %"FAgda.Builtin.Nat._-__23"
                                       i64 3, label %"FDownFromOpt.downFrom_23"]
"CDownFromOpt.List.[]_23":
  %5 = inttoptr i64 %x102 to ptr
  call fastcc void @free(ptr %5)
  ret void
"CDownFromOpt.List._∷__23":
  %6 = inttoptr i64 %x102 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x96 = load i64, ptr %7
  call fastcc void @"drop"(i64 %x96)
  %8 = inttoptr i64 %x102 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x95 = load i64, ptr %9
  call fastcc void @"drop"(i64 %x95)
  %10 = inttoptr i64 %x102 to ptr
  call fastcc void @free(ptr %10)
  ret void
"Cnat_23":
  %11 = inttoptr i64 %x102 to ptr
  call fastcc void @free(ptr %11)
  ret void
"FAgda.Builtin.Nat._-__23":
  %12 = inttoptr i64 %x102 to ptr
  %13 = getelementptr inbounds %Node, ptr %12, i32 0, i64 2
  %x98 = load i64, ptr %13
  call fastcc void @"drop"(i64 %x98)
  %14 = inttoptr i64 %x102 to ptr
  %15 = getelementptr inbounds %Node, ptr %14, i32 0, i64 3
  %x97 = load i64, ptr %15
  call fastcc void @"drop"(i64 %x97)
  %16 = inttoptr i64 %x102 to ptr
  call fastcc void @free(ptr %16)
  ret void
"FDownFromOpt.downFrom_23":
  %17 = inttoptr i64 %x102 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 2
  %x99 = load i64, ptr %18
  call fastcc void @"drop"(i64 %x99)
  %19 = inttoptr i64 %x102 to ptr
  call fastcc void @free(ptr %19)
  ret void
default_23:
  unreachable
default_22:
  %x94 = sub i64 %x101, 1
  %20 = inttoptr i64 %x102 to ptr
  %21 = getelementptr inbounds %Node, ptr %20, i32 0, i64 0
  store i64 %x94, ptr %21
  ret void
}

define fastcc void
@"dup"(i64 %x105){
  %1 = inttoptr i64 %x105 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x104 = load i64, ptr %2
  %x103 = add i64 %x104, 1
  %3 = inttoptr i64 %x105 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 0
  store i64 %x103, ptr %4
  ret void
}