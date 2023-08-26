target triple = "x86_64-unknown-linux-gnu"
declare void @printf(ptr, ...)
declare ptr @malloc(i64)
%Node = type [3 x i64]
@"%d" = private constant [4 x i8] c"%d\0A\00", align 1

; Tag numbering table:
; 0 Cnat
; 1 FAgda.Builtin.Nat._-_
; 2 CExample.List.[]
; 3 FExample.downFrom
; 4 CExample.List._∷_
; 5 FAgda.Builtin.Nat._+_
; 6 FExample.mapDouble
; 7 FExample.sum

define fastcc %Node
@"Example.downFrom"(i64 %x8){
  %1 = inttoptr i64 %x8 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x3111 = load i64, ptr %2
  switch i64 %x3111, label %default_0 [ i64 0, label %"Cnat_0"
                                        i64 1, label %"FAgda.Builtin.Nat._-__0" ]
"Cnat_0":
  %3 = inttoptr i64 %x8 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 1
  %x5557 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 0
  %"Cnat_0_res" = insertvalue %Node %5, i64 %x5557, 1
  br label %continue_0
"FAgda.Builtin.Nat._-__0":
  %6 = inttoptr i64 %x8 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 1
  %x5558 = load i64, ptr %7
  %8 = inttoptr i64 %x8 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 2
  %x5559 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__0_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x5558, i64 %x5559)
  br label %continue_0
default_0:
  unreachable
continue_0:
  %10 = phi %Node [%"Cnat_0_res", %"Cnat_0"], [%"FAgda.Builtin.Nat._-__0_res", %"FAgda.Builtin.Nat._-__0"]
  %x3110 = extractvalue %Node %10, 1
  %11 = insertvalue %Node undef, i64 0, 0
  %12 = insertvalue %Node %11, i64 %x3110, 1
  %13 = inttoptr i64 %x8 to ptr
  store %Node %12, ptr %13
  switch i64 %x3110, label %default_1 [ i64 0, label %"0_1" ]
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
@"Example.mapDouble"(i64 %x19){
  %1 = inttoptr i64 %x19 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x3123 = load i64, ptr %2
  switch i64 %x3123, label %default_2 [ i64 2, label %"CExample.List.[]_2"
                                        i64 4, label %"CExample.List._∷__2"
                                        i64 3, label %"FExample.downFrom_2" ]
"CExample.List.[]_2":
  %"CExample.List.[]_2_res" = insertvalue %Node undef, i64 2, 0
  br label %continue_2
"CExample.List._∷__2":
  %3 = inttoptr i64 %x19 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 1
  %x5560 = load i64, ptr %4
  %5 = inttoptr i64 %x19 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 2
  %x5562 = load i64, ptr %6
  %7 = insertvalue %Node undef, i64 4, 0
  %8 = insertvalue %Node %7, i64 %x5560, 1
  %"CExample.List._∷__2_res" = insertvalue %Node %8, i64 %x5562, 2
  br label %continue_2
"FExample.downFrom_2":
  %9 = inttoptr i64 %x19 to ptr
  %10 = getelementptr inbounds %Node, ptr %9, i32 0, i64 1
  %x5561 = load i64, ptr %10
  %"FExample.downFrom_2_res" = call
                               fastcc
                               %Node
                               @"Example.downFrom"(i64 %x5561)
  br label %continue_2
default_2:
  unreachable
continue_2:
  %11 = phi %Node [%"CExample.List.[]_2_res", %"CExample.List.[]_2"], [%"CExample.List._∷__2_res", %"CExample.List._∷__2"], [%"FExample.downFrom_2_res", %"FExample.downFrom_2"]
  %x3120 = extractvalue %Node %11, 0
  %x3121 = extractvalue %Node %11, 1
  %x3122 = extractvalue %Node %11, 2
  switch i64 %x3120, label %default_3 [ i64 2, label %"CExample.List.[]_3"
                                        i64 4, label %"CExample.List._∷__3" ]
"CExample.List.[]_3":
  %12 = insertvalue %Node undef, i64 %x3120, 0
  %13 = insertvalue %Node %12, i64 %x3121, 1
  %14 = insertvalue %Node %13, i64 %x3122, 2
  %15 = inttoptr i64 %x19 to ptr
  store %Node %14, ptr %15
  %16 = inttoptr i64 %x19 to ptr
  %17 = getelementptr inbounds %Node, ptr %16, i32 0, i64 0
  %x3117 = load i64, ptr %17
  switch i64 %x3117, label %default_4 [ i64 2, label %"CExample.List.[]_4"
                                        i64 4, label %"CExample.List._∷__4"
                                        i64 3, label %"FExample.downFrom_4" ]
"CExample.List.[]_4":
  %"CExample.List.[]_4_res" = insertvalue %Node undef, i64 2, 0
  br label %continue_4
"CExample.List._∷__4":
  %18 = inttoptr i64 %x19 to ptr
  %19 = getelementptr inbounds %Node, ptr %18, i32 0, i64 1
  %x5563 = load i64, ptr %19
  %20 = inttoptr i64 %x19 to ptr
  %21 = getelementptr inbounds %Node, ptr %20, i32 0, i64 2
  %x5565 = load i64, ptr %21
  %22 = insertvalue %Node undef, i64 4, 0
  %23 = insertvalue %Node %22, i64 %x5563, 1
  %"CExample.List._∷__4_res" = insertvalue %Node %23, i64 %x5565, 2
  br label %continue_4
"FExample.downFrom_4":
  %24 = inttoptr i64 %x19 to ptr
  %25 = getelementptr inbounds %Node, ptr %24, i32 0, i64 1
  %x5564 = load i64, ptr %25
  %"FExample.downFrom_4_res" = call
                               fastcc
                               %Node
                               @"Example.downFrom"(i64 %x5564)
  br label %continue_4
default_4:
  unreachable
continue_4:
  %26 = phi %Node [%"CExample.List.[]_4_res", %"CExample.List.[]_4"], [%"CExample.List._∷__4_res", %"CExample.List._∷__4"], [%"FExample.downFrom_4_res", %"FExample.downFrom_4"]
  %x3114 = extractvalue %Node %26, 0
  %x3115 = extractvalue %Node %26, 1
  %x3116 = extractvalue %Node %26, 2
  switch i64 %x3114, label %default_5 [ i64 2, label %"CExample.List.[]_5"
                                        i64 4, label %"CExample.List._∷__5"
                                        i64 3, label %"FExample.downFrom_5" ]
"CExample.List.[]_5":
  %27 = insertvalue %Node undef, i64 %x3114, 0
  %28 = insertvalue %Node %27, i64 %x3115, 1
  %29 = insertvalue %Node %28, i64 %x3116, 2
  %30 = inttoptr i64 %x19 to ptr
  store %Node %29, ptr %30
  %31 = insertvalue %Node undef, i64 2, 0
  ret %Node %31
"CExample.List._∷__5":
  %32 = insertvalue %Node undef, i64 %x3114, 0
  %33 = insertvalue %Node %32, i64 %x3115, 1
  %34 = insertvalue %Node %33, i64 %x3116, 2
  %35 = inttoptr i64 %x19 to ptr
  store %Node %34, ptr %35
  %36 = insertvalue %Node undef, i64 4, 0
  %37 = insertvalue %Node %36, i64 %x3115, 1
  %38 = insertvalue %Node %37, i64 %x3116, 2
  ret %Node %38
"FExample.downFrom_5":
  %39 = insertvalue %Node undef, i64 %x3114, 0
  %40 = insertvalue %Node %39, i64 %x3115, 1
  %41 = insertvalue %Node %40, i64 %x3116, 2
  %42 = inttoptr i64 %x19 to ptr
  store %Node %41, ptr %42
  %43 = call fastcc %Node @"Example.downFrom"(i64 %x3115)
  ret %Node %43
default_5:
  unreachable
"CExample.List._∷__3":
  %44 = insertvalue %Node undef, i64 %x3120, 0
  %45 = insertvalue %Node %44, i64 %x3121, 1
  %46 = insertvalue %Node %45, i64 %x3122, 2
  %47 = inttoptr i64 %x19 to ptr
  store %Node %46, ptr %47
  %48 = insertvalue %Node undef, i64 5, 0
  %49 = insertvalue %Node %48, i64 %x3121, 1
  %50 = insertvalue %Node %49, i64 %x3121, 2
  %51 = call fastcc ptr @malloc(i64 192)
  store %Node %50, ptr %51
  %x12 = ptrtoint ptr %51 to i64
  %52 = insertvalue %Node undef, i64 6, 0
  %53 = insertvalue %Node %52, i64 %x3122, 1
  %54 = call fastcc ptr @malloc(i64 192)
  store %Node %53, ptr %54
  %x14 = ptrtoint ptr %54 to i64
  %55 = insertvalue %Node undef, i64 4, 0
  %56 = insertvalue %Node %55, i64 %x12, 1
  %57 = insertvalue %Node %56, i64 %x14, 2
  ret %Node %57
default_3:
  unreachable
}

define fastcc %Node
@"Example.sum"(i64 %x27){
  %1 = inttoptr i64 %x27 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x3129 = load i64, ptr %2
  switch i64 %x3129, label %default_6 [ i64 2, label %"CExample.List.[]_6"
                                        i64 4, label %"CExample.List._∷__6"
                                        i64 3, label %"FExample.downFrom_6"
                                        i64 6, label %"FExample.mapDouble_6" ]
"CExample.List.[]_6":
  %"CExample.List.[]_6_res" = insertvalue %Node undef, i64 2, 0
  br label %continue_6
"CExample.List._∷__6":
  %3 = inttoptr i64 %x27 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 1
  %x5566 = load i64, ptr %4
  %5 = inttoptr i64 %x27 to ptr
  %6 = getelementptr inbounds %Node, ptr %5, i32 0, i64 2
  %x5569 = load i64, ptr %6
  %7 = insertvalue %Node undef, i64 4, 0
  %8 = insertvalue %Node %7, i64 %x5566, 1
  %"CExample.List._∷__6_res" = insertvalue %Node %8, i64 %x5569, 2
  br label %continue_6
"FExample.downFrom_6":
  %9 = inttoptr i64 %x27 to ptr
  %10 = getelementptr inbounds %Node, ptr %9, i32 0, i64 1
  %x5567 = load i64, ptr %10
  %"FExample.downFrom_6_res" = call
                               fastcc
                               %Node
                               @"Example.downFrom"(i64 %x5567)
  br label %continue_6
"FExample.mapDouble_6":
  %11 = inttoptr i64 %x27 to ptr
  %12 = getelementptr inbounds %Node, ptr %11, i32 0, i64 1
  %x5568 = load i64, ptr %12
  %"FExample.mapDouble_6_res" = call
                                fastcc
                                %Node
                                @"Example.mapDouble"(i64 %x5568)
  br label %continue_6
default_6:
  unreachable
continue_6:
  %13 = phi %Node [%"CExample.List.[]_6_res", %"CExample.List.[]_6"], [%"CExample.List._∷__6_res", %"CExample.List._∷__6"], [%"FExample.downFrom_6_res", %"FExample.downFrom_6"], [%"FExample.mapDouble_6_res", %"FExample.mapDouble_6"]
  %x3126 = extractvalue %Node %13, 0
  %x3127 = extractvalue %Node %13, 1
  %x3128 = extractvalue %Node %13, 2
  switch i64 %x3126, label %default_7 [ i64 2, label %"CExample.List.[]_7"
                                        i64 4, label %"CExample.List._∷__7" ]
"CExample.List.[]_7":
  %14 = insertvalue %Node undef, i64 %x3126, 0
  %15 = insertvalue %Node %14, i64 %x3127, 1
  %16 = insertvalue %Node %15, i64 %x3128, 2
  %17 = inttoptr i64 %x27 to ptr
  store %Node %16, ptr %17
  %18 = insertvalue %Node undef, i64 0, 0
  %19 = insertvalue %Node %18, i64 0, 1
  ret %Node %19
"CExample.List._∷__7":
  %20 = insertvalue %Node undef, i64 %x3126, 0
  %21 = insertvalue %Node %20, i64 %x3127, 1
  %22 = insertvalue %Node %21, i64 %x3128, 2
  %23 = inttoptr i64 %x27 to ptr
  store %Node %22, ptr %23
  %24 = insertvalue %Node undef, i64 7, 0
  %25 = insertvalue %Node %24, i64 %x3128, 1
  %26 = call fastcc ptr @malloc(i64 192)
  store %Node %25, ptr %26
  %x22 = ptrtoint ptr %26 to i64
  %27 = call
        fastcc
        %Node
        @"Agda.Builtin.Nat._+_"(i64 %x22, i64 %x3127)
  ret %Node %27
default_7:
  unreachable
}

define fastcc void
@main(){
  %1 = insertvalue %Node undef, i64 0, 0
  %2 = insertvalue %Node %1, i64 100, 1
  %3 = call fastcc ptr @malloc(i64 192)
  store %Node %2, ptr %3
  %x32 = ptrtoint ptr %3 to i64
  %4 = insertvalue %Node undef, i64 3, 0
  %5 = insertvalue %Node %4, i64 %x32, 1
  %6 = call fastcc ptr @malloc(i64 192)
  store %Node %5, ptr %6
  %x31 = ptrtoint ptr %6 to i64
  %7 = insertvalue %Node undef, i64 6, 0
  %8 = insertvalue %Node %7, i64 %x31, 1
  %9 = call fastcc ptr @malloc(i64 192)
  store %Node %8, ptr %9
  %x34 = ptrtoint ptr %9 to i64
  %10 = call fastcc %Node @"Example.sum"(i64 %x34)
  %x35 = extractvalue %Node %10, 1
  call fastcc void @printf(ptr @"%d", i64 %x35)
  ret void
}

define fastcc %Node
@"Agda.Builtin.Nat._+_"(i64 %x41, i64 %x42){
  %1 = inttoptr i64 %x41 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x3137 = load i64, ptr %2
  switch i64 %x3137, label %default_8 [ i64 0, label %"Cnat_8"
                                        i64 1, label %"FAgda.Builtin.Nat._-__8"
                                        i64 7, label %"FExample.sum_8" ]
"Cnat_8":
  %3 = inttoptr i64 %x41 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 1
  %x5570 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 0
  %"Cnat_8_res" = insertvalue %Node %5, i64 %x5570, 1
  br label %continue_8
"FAgda.Builtin.Nat._-__8":
  %6 = inttoptr i64 %x41 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 1
  %x5571 = load i64, ptr %7
  %8 = inttoptr i64 %x41 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 2
  %x5573 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__8_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x5571, i64 %x5573)
  br label %continue_8
"FExample.sum_8":
  %10 = inttoptr i64 %x41 to ptr
  %11 = getelementptr inbounds %Node, ptr %10, i32 0, i64 1
  %x5572 = load i64, ptr %11
  %"FExample.sum_8_res" = call
                          fastcc
                          %Node
                          @"Example.sum"(i64 %x5572)
  br label %continue_8
default_8:
  unreachable
continue_8:
  %12 = phi %Node [%"Cnat_8_res", %"Cnat_8"], [%"FAgda.Builtin.Nat._-__8_res", %"FAgda.Builtin.Nat._-__8"], [%"FExample.sum_8_res", %"FExample.sum_8"]
  %x3136 = extractvalue %Node %12, 1
  %13 = insertvalue %Node undef, i64 0, 0
  %14 = insertvalue %Node %13, i64 %x3136, 1
  %15 = inttoptr i64 %x41 to ptr
  store %Node %14, ptr %15
  %16 = inttoptr i64 %x42 to ptr
  %17 = getelementptr inbounds %Node, ptr %16, i32 0, i64 0
  %x3133 = load i64, ptr %17
  switch i64 %x3133, label %default_9 [ i64 0, label %"Cnat_9"
                                        i64 5, label %"FAgda.Builtin.Nat._+__9"
                                        i64 1, label %"FAgda.Builtin.Nat._-__9" ]
"Cnat_9":
  %18 = inttoptr i64 %x42 to ptr
  %19 = getelementptr inbounds %Node, ptr %18, i32 0, i64 1
  %x5574 = load i64, ptr %19
  %20 = insertvalue %Node undef, i64 0, 0
  %"Cnat_9_res" = insertvalue %Node %20, i64 %x5574, 1
  br label %continue_9
"FAgda.Builtin.Nat._+__9":
  %21 = inttoptr i64 %x42 to ptr
  %22 = getelementptr inbounds %Node, ptr %21, i32 0, i64 1
  %x5575 = load i64, ptr %22
  %23 = inttoptr i64 %x42 to ptr
  %24 = getelementptr inbounds %Node, ptr %23, i32 0, i64 2
  %x5577 = load i64, ptr %24
  %"FAgda.Builtin.Nat._+__9_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._+_"(i64 %x5575, i64 %x5577)
  br label %continue_9
"FAgda.Builtin.Nat._-__9":
  %25 = inttoptr i64 %x42 to ptr
  %26 = getelementptr inbounds %Node, ptr %25, i32 0, i64 1
  %x5576 = load i64, ptr %26
  %27 = inttoptr i64 %x42 to ptr
  %28 = getelementptr inbounds %Node, ptr %27, i32 0, i64 2
  %x5578 = load i64, ptr %28
  %"FAgda.Builtin.Nat._-__9_res" = call
                                   fastcc
                                   %Node
                                   @"Agda.Builtin.Nat._-_"(i64 %x5576, i64 %x5578)
  br label %continue_9
default_9:
  unreachable
continue_9:
  %29 = phi %Node [%"Cnat_9_res", %"Cnat_9"], [%"FAgda.Builtin.Nat._+__9_res", %"FAgda.Builtin.Nat._+__9"], [%"FAgda.Builtin.Nat._-__9_res", %"FAgda.Builtin.Nat._-__9"]
  %x3132 = extractvalue %Node %29, 1
  %30 = insertvalue %Node undef, i64 0, 0
  %31 = insertvalue %Node %30, i64 %x3132, 1
  %32 = inttoptr i64 %x42 to ptr
  store %Node %31, ptr %32
  %x38 = add i64 %x3136, %x3132
  %33 = insertvalue %Node undef, i64 0, 0
  %34 = insertvalue %Node %33, i64 %x38, 1
  ret %Node %34
}

define fastcc %Node
@"Agda.Builtin.Nat._-_"(i64 %x48, i64 %x49){
  %1 = inttoptr i64 %x48 to ptr
  %2 = getelementptr inbounds %Node, ptr %1, i32 0, i64 0
  %x3142 = load i64, ptr %2
  switch i64 %x3142, label %default_10 [ i64 0, label %"Cnat_10"
                                         i64 1, label %"FAgda.Builtin.Nat._-__10" ]
"Cnat_10":
  %3 = inttoptr i64 %x48 to ptr
  %4 = getelementptr inbounds %Node, ptr %3, i32 0, i64 1
  %x5579 = load i64, ptr %4
  %5 = insertvalue %Node undef, i64 0, 0
  %"Cnat_10_res" = insertvalue %Node %5, i64 %x5579, 1
  br label %continue_10
"FAgda.Builtin.Nat._-__10":
  %6 = inttoptr i64 %x48 to ptr
  %7 = getelementptr inbounds %Node, ptr %6, i32 0, i64 1
  %x5580 = load i64, ptr %7
  %8 = inttoptr i64 %x48 to ptr
  %9 = getelementptr inbounds %Node, ptr %8, i32 0, i64 2
  %x5581 = load i64, ptr %9
  %"FAgda.Builtin.Nat._-__10_res" = call
                                    fastcc
                                    %Node
                                    @"Agda.Builtin.Nat._-_"(i64 %x5580, i64 %x5581)
  br label %continue_10
default_10:
  unreachable
continue_10:
  %10 = phi %Node [%"Cnat_10_res", %"Cnat_10"], [%"FAgda.Builtin.Nat._-__10_res", %"FAgda.Builtin.Nat._-__10"]
  %x3141 = extractvalue %Node %10, 1
  %11 = insertvalue %Node undef, i64 0, 0
  %12 = insertvalue %Node %11, i64 %x3141, 1
  %13 = inttoptr i64 %x48 to ptr
  store %Node %12, ptr %13
  %14 = inttoptr i64 %x49 to ptr
  %15 = getelementptr inbounds %Node, ptr %14, i32 0, i64 1
  %x91 = load i64, ptr %15
  %16 = insertvalue %Node undef, i64 0, 0
  %17 = insertvalue %Node %16, i64 %x91, 1
  %18 = inttoptr i64 %x49 to ptr
  store %Node %17, ptr %18
  %x45 = sub i64 %x3141, %x91
  %19 = insertvalue %Node undef, i64 0, 0
  %20 = insertvalue %Node %19, i64 %x45, 1
  ret %Node %20
}