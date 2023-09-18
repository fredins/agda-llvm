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
  %x72 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 1
  %"Cnat_0_res" = insertvalue %Node %5, i64 %x72, 2
  br label %continue_0
"FAgda.Builtin.Nat._-__0":
  %6 = inttoptr i64 %x7 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x73 = load i64, ptr %7
  %8 = inttoptr i64 %x7 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x74 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__0_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x73, i64 %x74)
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
  call fastcc void @"drop"(i64 %x7)
  %17 = insertvalue %Node undef, i64 2, 1
  ret %Node %17
default_1:
  %18 = insertvalue %Node undef, i64 0, 1
  %19 = insertvalue %Node %18, i64 1, 2
  %20 = insertvalue %Node %19, i64 1, 0
  %21 = call fastcc ptr @malloc(i64 32)
  store %Node %20, ptr %21
  %x5 = ptrtoint ptr %21 to i64
  %22 = insertvalue %Node undef, i64 1, 1
  %23 = insertvalue %Node %22, i64 %x7, 2
  %24 = insertvalue %Node %23, i64 %x5, 3
  %25 = insertvalue %Node %24, i64 1, 0
  %26 = call fastcc ptr @malloc(i64 32)
  store %Node %25, ptr %26
  %x4 = ptrtoint ptr %26 to i64
  call fastcc void @"dup"(i64 %x4)
  %27 = insertvalue %Node undef, i64 3, 1
  %28 = insertvalue %Node %27, i64 %x4, 2
  %29 = insertvalue %Node %28, i64 1, 0
  %30 = call fastcc ptr @malloc(i64 32)
  store %Node %29, ptr %30
  %x3 = ptrtoint ptr %30 to i64
  call fastcc void @"dup"(i64 %x3)
  call fastcc void @"dup"(i64 %x4)
  %31 = insertvalue %Node undef, i64 4, 1
  %32 = insertvalue %Node %31, i64 %x4, 2
  %33 = insertvalue %Node %32, i64 %x3, 3
  ret %Node %33
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
  switch i64 %x59, label %default_2 [i64 2, label %"CDownFrom.List.[]_2"
                                     i64 4, label %"CDownFrom.List._∷__2"]
"CDownFrom.List.[]_2":
  %4 = insertvalue %Node undef, i64 %x59, 1
  %5 = insertvalue %Node %4, i64 %x60, 2
  %6 = insertvalue %Node %5, i64 %x61, 3
  %7 = inttoptr i64 %x14 to ptr
  %8 = getelementptr inbounds %Node, ptr %7, i32 0, i64 0
  %9 = load i64, ptr %8
  %10 = insertvalue %Node %6, i64 %9, 0
  store %Node %10, ptr %7
  call fastcc void @"drop"(i64 %x14)
  %11 = insertvalue %Node undef, i64 0, 1
  %12 = insertvalue %Node %11, i64 0, 2
  ret %Node %12
"CDownFrom.List._∷__2":
  %13 = insertvalue %Node undef, i64 %x59, 1
  %14 = insertvalue %Node %13, i64 %x60, 2
  %15 = insertvalue %Node %14, i64 %x61, 3
  %16 = inttoptr i64 %x14 to ptr
  %17 = getelementptr inbounds %Node, ptr %16, i32 0, i64 0
  %18 = load i64, ptr %17
  %19 = insertvalue %Node %15, i64 %18, 0
  store %Node %19, ptr %16
  call fastcc void @"drop"(i64 %x14)
  %20 = insertvalue %Node undef, i64 5, 1
  %21 = insertvalue %Node %20, i64 %x61, 2
  %22 = insertvalue %Node %21, i64 1, 0
  %23 = call fastcc ptr @malloc(i64 32)
  store %Node %22, ptr %23
  %x10 = ptrtoint ptr %23 to i64
  %24 = tail call
        fastcc
        %Node
        @"Agda.Builtin.Nat._+_"(i64 %x10, i64 %x60)
  ret %Node %24
default_2:
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
  call fastcc void @"drop"(i64 %x25)
  %10 = inttoptr i64 %x26 to ptr
  %11 = getelementptr inbounds %Node, ptr %10, i32 0, i64 1
  %x63 = load i64, ptr %11
  switch i64 %x63, label %default_3 [i64 0, label %"Cnat_3"
                                     i64 1, label %"FAgda.Builtin.Nat._-__3"]
"Cnat_3":
  %12 = inttoptr i64 %x26 to ptr
  %13 = getelementptr inbounds %Node, ptr %12, i32 0, i64 2
  %x75 = load i64, ptr %13
  %14 = insertvalue %Node undef, i64 0, 1
  %"Cnat_3_res" = insertvalue %Node %14, i64 %x75, 2
  br label %continue_3
"FAgda.Builtin.Nat._-__3":
  %15 = inttoptr i64 %x26 to ptr
  %16 = getelementptr inbounds %Node, ptr %15, i32 0, i64 2
  %x76 = load i64, ptr %16
  %17 = inttoptr i64 %x26 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 3
  %x77 = load i64, ptr %18
  %"FAgda.Builtin.Nat._-__3_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x76, i64 %x77)
  br label %continue_3
default_3:
  unreachable
continue_3:
  %19 = phi %Node  [%"Cnat_3_res", %"Cnat_3"]
                 , [%"FAgda.Builtin.Nat._-__3_res", %"FAgda.Builtin.Nat._-__3"]
  %x62 = extractvalue %Node %19, 2
  %20 = insertvalue %Node undef, i64 0, 1
  %21 = insertvalue %Node %20, i64 %x62, 2
  %22 = inttoptr i64 %x26 to ptr
  %23 = getelementptr inbounds %Node, ptr %22, i32 0, i64 0
  %24 = load i64, ptr %23
  %25 = insertvalue %Node %21, i64 %24, 0
  store %Node %25, ptr %22
  call fastcc void @"drop"(i64 %x26)
  %x24 = add i64 %x66, %x62
  %26 = insertvalue %Node undef, i64 0, 1
  %27 = insertvalue %Node %26, i64 %x24, 2
  ret %Node %27
}

define fastcc %Node
@"Agda.Builtin.Nat._-_"(i64 %x31, i64 %x32){
  %1 = inttoptr i64 %x31 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 1
  %x69 = load i64, ptr %2
  switch i64 %x69, label %default_4 [i64 0, label %"Cnat_4"
                                     i64 1, label %"FAgda.Builtin.Nat._-__4"]
"Cnat_4":
  %3 = inttoptr i64 %x31 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 2
  %x78 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 1
  %"Cnat_4_res" = insertvalue %Node %5, i64 %x78, 2
  br label %continue_4
"FAgda.Builtin.Nat._-__4":
  %6 = inttoptr i64 %x31 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x79 = load i64, ptr %7
  %8 = inttoptr i64 %x31 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x80 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__4_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x79, i64 %x80)
  br label %continue_4
default_4:
  unreachable
continue_4:
  %10 = phi %Node  [%"Cnat_4_res", %"Cnat_4"]
                 , [%"FAgda.Builtin.Nat._-__4_res", %"FAgda.Builtin.Nat._-__4"]
  %x68 = extractvalue %Node %10, 2
  %11 = insertvalue %Node undef, i64 0, 1
  %12 = insertvalue %Node %11, i64 %x68, 2
  %13 = inttoptr i64 %x31 to ptr
  %14 = getelementptr inbounds %Node, ptr %13, i32 0, i64 0
  %15 = load i64, ptr %14
  %16 = insertvalue %Node %12, i64 %15, 0
  store %Node %16, ptr %13
  call fastcc void @"drop"(i64 %x31)
  %17 = inttoptr i64 %x32 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 2
  %x54 = load i64, ptr %18
  %19 = insertvalue %Node undef, i64 0, 1
  %20 = insertvalue %Node %19, i64 %x54, 2
  %21 = inttoptr i64 %x32 to ptr
  %22 = getelementptr inbounds %Node, ptr %21, i32 0, i64 0
  %23 = load i64, ptr %22
  %24 = insertvalue %Node %20, i64 %23, 0
  store %Node %24, ptr %21
  call fastcc void @"drop"(i64 %x32)
  %x30 = sub i64 %x68, %x54
  %25 = insertvalue %Node undef, i64 0, 1
  %26 = insertvalue %Node %25, i64 %x30, 2
  ret %Node %26
}

define fastcc void
@"drop"(i64 %x93){
  %1 = inttoptr i64 %x93 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x92 = load i64, ptr %2
  switch i64 %x92, label %default_5 [i64 1, label %"1_5"]
"1_5":
  %3 = inttoptr i64 %x93 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 1
  %x91 = load i64, ptr %4
  switch i64 %x91, label %default_6 [i64 2, label %"CDownFrom.List.[]_6"
                                     i64 4, label %"CDownFrom.List._∷__6"
                                     i64 0, label %"Cnat_6"
                                     i64 1, label %"FAgda.Builtin.Nat._-__6"
                                     i64 3, label %"FDownFrom.downFrom_6"
                                     i64 5, label %"FDownFrom.sum_6"]
"CDownFrom.List.[]_6":
  %5 = inttoptr i64 %x93 to ptr
  call fastcc void @free(ptr %5)
  ret void
"CDownFrom.List._∷__6":
  %6 = inttoptr i64 %x93 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 2
  %x86 = load i64, ptr %7
  call fastcc void @"drop"(i64 %x86)
  %8 = inttoptr i64 %x93 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 3
  %x85 = load i64, ptr %9
  call fastcc void @"drop"(i64 %x85)
  %10 = inttoptr i64 %x93 to ptr
  call fastcc void @free(ptr %10)
  ret void
"Cnat_6":
  %11 = inttoptr i64 %x93 to ptr
  call fastcc void @free(ptr %11)
  ret void
"FAgda.Builtin.Nat._-__6":
  %12 = inttoptr i64 %x93 to ptr
  %13 = getelementptr inbounds %Node, ptr %12, i32 0, i64 2
  %x88 = load i64, ptr %13
  call fastcc void @"drop"(i64 %x88)
  %14 = inttoptr i64 %x93 to ptr
  %15 = getelementptr inbounds %Node, ptr %14, i32 0, i64 3
  %x87 = load i64, ptr %15
  call fastcc void @"drop"(i64 %x87)
  %16 = inttoptr i64 %x93 to ptr
  call fastcc void @free(ptr %16)
  ret void
"FDownFrom.downFrom_6":
  %17 = inttoptr i64 %x93 to ptr
  %18 = getelementptr inbounds %Node, ptr %17, i32 0, i64 2
  %x89 = load i64, ptr %18
  call fastcc void @"drop"(i64 %x89)
  %19 = inttoptr i64 %x93 to ptr
  call fastcc void @free(ptr %19)
  ret void
"FDownFrom.sum_6":
  %20 = inttoptr i64 %x93 to ptr
  %21 = getelementptr inbounds %Node, ptr %20, i32 0, i64 2
  %x90 = load i64, ptr %21
  call fastcc void @"drop"(i64 %x90)
  %22 = inttoptr i64 %x93 to ptr
  call fastcc void @free(ptr %22)
  ret void
default_6:
  unreachable
default_5:
  %x84 = sub i64 %x92, 1
  %23 = inttoptr i64 %x93 to ptr
  %24 = getelementptr inbounds %Node, ptr %23, i32 0, i64 0
  store i64 %x84, ptr %24
  ret void
}

define fastcc void
@"dup"(i64 %x83){
  %1 = inttoptr i64 %x83 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x82 = load i64, ptr %2
  %x81 = add i64 %x82, 1
  %3 = inttoptr i64 %x83 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 0
  store i64 %x81, ptr %4
  ret void
}