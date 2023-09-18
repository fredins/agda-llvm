; ModuleID = 'llvm/program.ll'
source_filename = "llvm/program.ll"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@"%d" = private constant [4 x i8] c"%d\0A\00", align 1

; Function Attrs: nofree nounwind
declare void @printf(ptr nocapture noundef readonly, ...) local_unnamed_addr #0

; Function Attrs: inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0)
declare noalias noundef ptr @malloc(i64 noundef) local_unnamed_addr #1

; Function Attrs: inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free")
declare void @free(ptr allocptr nocapture noundef) local_unnamed_addr #2

; Function Attrs: nounwind
define fastcc [4 x i64] @"Agda.Builtin.Nat._+_"(i64 %x3, i64 %x4) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x3 to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x82 = load i64, ptr %2, align 4
  %switch = icmp eq i64 %x82, 0
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x110 = load i64, ptr %3, align 4
  br i1 %switch, label %Cnat_0, label %"FAgda.Builtin.Nat._+__0"

Cnat_0:                                           ; preds = %0
  %Cnat_0_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x110, 2
  br label %continue_0

"FAgda.Builtin.Nat._+__0":                        ; preds = %0
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x112 = load i64, ptr %4, align 4
  %"FAgda.Builtin.Nat._+__0_res" = tail call fastcc [4 x i64] @"Agda.Builtin.Nat._+_"(i64 %x110, i64 %x112)
  br label %continue_0

continue_0:                                       ; preds = %"FAgda.Builtin.Nat._+__0", %Cnat_0
  %5 = phi [4 x i64] [ %Cnat_0_res, %Cnat_0 ], [ %"FAgda.Builtin.Nat._+__0_res", %"FAgda.Builtin.Nat._+__0" ]
  %x81 = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %2, align 4
  %.repack3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x81, ptr %.repack3, align 4
  tail call fastcc void @drop(i64 %x3)
  %6 = inttoptr i64 %x4 to ptr
  %7 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 1
  %x78 = load i64, ptr %7, align 4
  %switch13 = icmp eq i64 %x78, 0
  %8 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  %x113 = load i64, ptr %8, align 4
  br i1 %switch13, label %Cnat_1, label %FAgda.Builtin.Nat._-__1

Cnat_1:                                           ; preds = %continue_0
  %Cnat_1_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x113, 2
  br label %continue_1

FAgda.Builtin.Nat._-__1:                          ; preds = %continue_0
  %9 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 3
  %x115 = load i64, ptr %9, align 4
  %FAgda.Builtin.Nat._-__1_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x113, i64 %x115)
  br label %continue_1

continue_1:                                       ; preds = %FAgda.Builtin.Nat._-__1, %Cnat_1
  %10 = phi [4 x i64] [ %Cnat_1_res, %Cnat_1 ], [ %FAgda.Builtin.Nat._-__1_res, %FAgda.Builtin.Nat._-__1 ]
  %x77 = extractvalue [4 x i64] %10, 2
  store i64 0, ptr %7, align 4
  %.repack9 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  store i64 %x77, ptr %.repack9, align 4
  tail call fastcc void @drop(i64 %x4)
  %x2 = add i64 %x77, %x81
  %11 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x2, 2
  ret [4 x i64] %11
}

; Function Attrs: nounwind
define fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x9, i64 %x10) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x9 to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x87 = load i64, ptr %2, align 4
  %switch = icmp eq i64 %x87, 0
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x116 = load i64, ptr %3, align 4
  br i1 %switch, label %Cnat_2, label %FAgda.Builtin.Nat._-__2

Cnat_2:                                           ; preds = %0
  %Cnat_2_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x116, 2
  br label %continue_2

FAgda.Builtin.Nat._-__2:                          ; preds = %0
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x118 = load i64, ptr %4, align 4
  %FAgda.Builtin.Nat._-__2_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x116, i64 %x118)
  br label %continue_2

continue_2:                                       ; preds = %FAgda.Builtin.Nat._-__2, %Cnat_2
  %5 = phi [4 x i64] [ %Cnat_2_res, %Cnat_2 ], [ %FAgda.Builtin.Nat._-__2_res, %FAgda.Builtin.Nat._-__2 ]
  %x86 = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %2, align 4
  %.repack3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x86, ptr %.repack3, align 4
  tail call fastcc void @drop(i64 %x9)
  %6 = inttoptr i64 %x10 to ptr
  %7 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  %x56 = load i64, ptr %7, align 4
  %.repack7 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 1
  store i64 0, ptr %.repack7, align 4
  tail call fastcc void @drop(i64 %x10)
  %x8 = sub i64 %x86, %x56
  %8 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x8, 2
  ret [4 x i64] %8
}

; Function Attrs: nounwind
define fastcc [4 x i64] @DownFromTail.downFrom(i64 %x19, i64 %x20) local_unnamed_addr #3 {
  br label %tailrecurse

tailrecurse:                                      ; preds = %default_4, %0
  %x19.tr = phi i64 [ %x19, %0 ], [ %x15, %default_4 ]
  %x20.tr = phi i64 [ %x20, %0 ], [ %x16, %default_4 ]
  %1 = inttoptr i64 %x20.tr to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x97 = load i64, ptr %2, align 4
  %switch = icmp eq i64 %x97, 0
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x119 = load i64, ptr %3, align 4
  br i1 %switch, label %Cnat_3, label %FAgda.Builtin.Nat._-__3

Cnat_3:                                           ; preds = %tailrecurse
  %Cnat_3_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x119, 2
  br label %continue_3

FAgda.Builtin.Nat._-__3:                          ; preds = %tailrecurse
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x121 = load i64, ptr %4, align 4
  %FAgda.Builtin.Nat._-__3_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x119, i64 %x121)
  br label %continue_3

continue_3:                                       ; preds = %FAgda.Builtin.Nat._-__3, %Cnat_3
  %5 = phi [4 x i64] [ %Cnat_3_res, %Cnat_3 ], [ %FAgda.Builtin.Nat._-__3_res, %FAgda.Builtin.Nat._-__3 ]
  %x96 = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %2, align 4
  %.repack3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x96, ptr %.repack3, align 4
  %cond = icmp eq i64 %x96, 0
  br i1 %cond, label %"0_4", label %default_4

"0_4":                                            ; preds = %continue_3
  tail call fastcc void @drop(i64 %x20.tr)
  %6 = inttoptr i64 %x19.tr to ptr
  %7 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 1
  %x93 = load i64, ptr %7, align 4
  %switch34 = icmp eq i64 %x93, 3
  br i1 %switch34, label %continue_5, label %"CDownFromTail.List._\E2\88\B7__5"

"CDownFromTail.List._\E2\88\B7__5":               ; preds = %"0_4"
  %8 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  %x122 = load i64, ptr %8, align 4
  %9 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 3
  %x123 = load i64, ptr %9, align 4
  %10 = inttoptr i64 %x123 to ptr
  %x132.i = load i64, ptr %10, align 4
  %x131.i = add i64 %x132.i, 1
  store i64 %x131.i, ptr %10, align 4
  %11 = inttoptr i64 %x122 to ptr
  %x132.i36 = load i64, ptr %11, align 4
  %x131.i37 = add i64 %x132.i36, 1
  store i64 %x131.i37, ptr %11, align 4
  %12 = insertvalue [4 x i64] [i64 undef, i64 4, i64 undef, i64 undef], i64 %x122, 2
  %"CDownFromTail.List._\E2\88\B7__5_res" = insertvalue [4 x i64] %12, i64 %x123, 3
  br label %continue_5

continue_5:                                       ; preds = %"0_4", %"CDownFromTail.List._\E2\88\B7__5"
  %13 = phi [4 x i64] [ %"CDownFromTail.List._\E2\88\B7__5_res", %"CDownFromTail.List._\E2\88\B7__5" ], [ [i64 undef, i64 3, i64 undef, i64 undef], %"0_4" ]
  %x90 = extractvalue [4 x i64] %13, 1
  %switch35 = icmp eq i64 %x90, 3
  br i1 %switch35, label %"CDownFromTail.List.[]_6", label %"CDownFromTail.List._\E2\88\B7__6"

common.ret:                                       ; preds = %"CDownFromTail.List._\E2\88\B7__6", %"CDownFromTail.List.[]_6"
  %common.ret.op = phi [4 x i64] [ [i64 undef, i64 3, i64 undef, i64 undef], %"CDownFromTail.List.[]_6" ], [ %17, %"CDownFromTail.List._\E2\88\B7__6" ]
  ret [4 x i64] %common.ret.op

"CDownFromTail.List.[]_6":                        ; preds = %continue_5
  store i64 3, ptr %7, align 4
  %.repack30 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  %.elt31 = extractvalue [4 x i64] %13, 2
  store i64 %.elt31, ptr %.repack30, align 4
  %.repack32 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 3
  %.elt33 = extractvalue [4 x i64] %13, 3
  store i64 %.elt33, ptr %.repack32, align 4
  tail call fastcc void @drop(i64 %x19.tr)
  br label %common.ret

"CDownFromTail.List._\E2\88\B7__6":               ; preds = %continue_5
  %x92 = extractvalue [4 x i64] %13, 3
  %x91 = extractvalue [4 x i64] %13, 2
  store i64 %x90, ptr %7, align 4
  %.repack24 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  store i64 %x91, ptr %.repack24, align 4
  %.repack26 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 3
  store i64 %x92, ptr %.repack26, align 4
  tail call fastcc void @drop(i64 %x19.tr)
  %14 = inttoptr i64 %x92 to ptr
  %x132.i38 = load i64, ptr %14, align 4
  %x131.i39 = add i64 %x132.i38, 1
  store i64 %x131.i39, ptr %14, align 4
  %15 = inttoptr i64 %x91 to ptr
  %x132.i40 = load i64, ptr %15, align 4
  %x131.i41 = add i64 %x132.i40, 1
  store i64 %x131.i41, ptr %15, align 4
  %16 = insertvalue [4 x i64] [i64 undef, i64 4, i64 undef, i64 undef], i64 %x91, 2
  %17 = insertvalue [4 x i64] %16, i64 %x92, 3
  br label %common.ret

default_4:                                        ; preds = %continue_3
  %18 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 0>, ptr %18, align 4
  %.repack8 = getelementptr inbounds [4 x i64], ptr %18, i64 0, i64 2
  store i64 1, ptr %.repack8, align 4
  %x17 = ptrtoint ptr %18 to i64
  %19 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  %.repack12 = getelementptr inbounds [4 x i64], ptr %19, i64 0, i64 2
  store i64 %x20.tr, ptr %.repack12, align 4
  %.repack14 = getelementptr inbounds [4 x i64], ptr %19, i64 0, i64 3
  store i64 %x17, ptr %.repack14, align 4
  %x16 = ptrtoint ptr %19 to i64
  store <2 x i64> <i64 2, i64 2>, ptr %19, align 4
  %20 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 4>, ptr %20, align 4
  %.repack18 = getelementptr inbounds [4 x i64], ptr %20, i64 0, i64 2
  store i64 %x16, ptr %.repack18, align 4
  %.repack20 = getelementptr inbounds [4 x i64], ptr %20, i64 0, i64 3
  store i64 %x19.tr, ptr %.repack20, align 4
  %x15 = ptrtoint ptr %20 to i64
  br label %tailrecurse
}

; Function Attrs: nounwind
define fastcc [4 x i64] @DownFromTail.sum(i64 %x27, i64 %x28) local_unnamed_addr #3 {
  br label %tailrecurse

tailrecurse:                                      ; preds = %"CDownFromTail.List._\E2\88\B7__8", %0
  %x27.tr = phi i64 [ %x27, %0 ], [ %x23, %"CDownFromTail.List._\E2\88\B7__8" ]
  %x28.tr = phi i64 [ %x28, %0 ], [ %x106, %"CDownFromTail.List._\E2\88\B7__8" ]
  %1 = inttoptr i64 %x28.tr to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x107 = load i64, ptr %2, align 4
  switch i64 %x107, label %default_7 [
    i64 3, label %continue_7
    i64 4, label %"CDownFromTail.List._\E2\88\B7__7"
    i64 5, label %FDownFromTail.downFrom_7
  ]

"CDownFromTail.List._\E2\88\B7__7":               ; preds = %tailrecurse
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x124 = load i64, ptr %3, align 4
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x126 = load i64, ptr %4, align 4
  %5 = inttoptr i64 %x126 to ptr
  %x132.i = load i64, ptr %5, align 4
  %x131.i = add i64 %x132.i, 1
  store i64 %x131.i, ptr %5, align 4
  %6 = inttoptr i64 %x124 to ptr
  %x132.i26 = load i64, ptr %6, align 4
  %x131.i27 = add i64 %x132.i26, 1
  store i64 %x131.i27, ptr %6, align 4
  %7 = insertvalue [4 x i64] [i64 undef, i64 4, i64 undef, i64 undef], i64 %x124, 2
  %"CDownFromTail.List._\E2\88\B7__7_res" = insertvalue [4 x i64] %7, i64 %x126, 3
  br label %continue_7

FDownFromTail.downFrom_7:                         ; preds = %tailrecurse
  %8 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x125 = load i64, ptr %8, align 4
  %9 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x127 = load i64, ptr %9, align 4
  %FDownFromTail.downFrom_7_res = tail call fastcc [4 x i64] @DownFromTail.downFrom(i64 %x125, i64 %x127)
  br label %continue_7

default_7:                                        ; preds = %tailrecurse
  unreachable

continue_7:                                       ; preds = %tailrecurse, %FDownFromTail.downFrom_7, %"CDownFromTail.List._\E2\88\B7__7"
  %10 = phi [4 x i64] [ %"CDownFromTail.List._\E2\88\B7__7_res", %"CDownFromTail.List._\E2\88\B7__7" ], [ %FDownFromTail.downFrom_7_res, %FDownFromTail.downFrom_7 ], [ [i64 undef, i64 3, i64 undef, i64 undef], %tailrecurse ]
  %x104 = extractvalue [4 x i64] %10, 1
  %switch = icmp eq i64 %x104, 3
  br i1 %switch, label %"CDownFromTail.List.[]_8", label %"CDownFromTail.List._\E2\88\B7__8"

"CDownFromTail.List.[]_8":                        ; preds = %continue_7
  %11 = inttoptr i64 %x28.tr to ptr
  store i64 3, ptr %2, align 4
  %.repack15 = getelementptr inbounds [4 x i64], ptr %11, i64 0, i64 2
  %.elt16 = extractvalue [4 x i64] %10, 2
  store i64 %.elt16, ptr %.repack15, align 4
  %.repack17 = getelementptr inbounds [4 x i64], ptr %11, i64 0, i64 3
  %.elt18 = extractvalue [4 x i64] %10, 3
  store i64 %.elt18, ptr %.repack17, align 4
  tail call fastcc void @drop(i64 %x28.tr)
  %12 = inttoptr i64 %x27.tr to ptr
  %13 = getelementptr inbounds [4 x i64], ptr %12, i64 0, i64 1
  %x101 = load i64, ptr %13, align 4
  %switch25 = icmp eq i64 %x101, 0
  %14 = getelementptr inbounds [4 x i64], ptr %12, i64 0, i64 2
  %x128 = load i64, ptr %14, align 4
  br i1 %switch25, label %Cnat_9, label %"FAgda.Builtin.Nat._+__9"

Cnat_9:                                           ; preds = %"CDownFromTail.List.[]_8"
  %Cnat_9_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x128, 2
  br label %continue_9

"FAgda.Builtin.Nat._+__9":                        ; preds = %"CDownFromTail.List.[]_8"
  %15 = getelementptr inbounds [4 x i64], ptr %12, i64 0, i64 3
  %x130 = load i64, ptr %15, align 4
  %"FAgda.Builtin.Nat._+__9_res" = tail call fastcc [4 x i64] @"Agda.Builtin.Nat._+_"(i64 %x128, i64 %x130)
  br label %continue_9

continue_9:                                       ; preds = %"FAgda.Builtin.Nat._+__9", %Cnat_9
  %16 = phi [4 x i64] [ %Cnat_9_res, %Cnat_9 ], [ %"FAgda.Builtin.Nat._+__9_res", %"FAgda.Builtin.Nat._+__9" ]
  %x100 = extractvalue [4 x i64] %16, 2
  %17 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x100, 2
  store i64 0, ptr %13, align 4
  %.repack21 = getelementptr inbounds [4 x i64], ptr %12, i64 0, i64 2
  store i64 %x100, ptr %.repack21, align 4
  tail call fastcc void @drop(i64 %x27.tr)
  ret [4 x i64] %17

"CDownFromTail.List._\E2\88\B7__8":               ; preds = %continue_7
  %x106 = extractvalue [4 x i64] %10, 3
  %x105 = extractvalue [4 x i64] %10, 2
  store i64 %x104, ptr %2, align 4
  %.repack3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x105, ptr %.repack3, align 4
  %.repack5 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  store i64 %x106, ptr %.repack5, align 4
  tail call fastcc void @drop(i64 %x28.tr)
  %18 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 1>, ptr %18, align 4
  %.repack9 = getelementptr inbounds [4 x i64], ptr %18, i64 0, i64 2
  store i64 %x27.tr, ptr %.repack9, align 4
  %.repack11 = getelementptr inbounds [4 x i64], ptr %18, i64 0, i64 3
  store i64 %x105, ptr %.repack11, align 4
  %x23 = ptrtoint ptr %18 to i64
  br label %tailrecurse
}

define fastcc void @main() local_unnamed_addr {
  %1 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store i64 1, ptr %1, align 4
  %.repack1 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x38 = ptrtoint ptr %1 to i64
  %2 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  tail call void @llvm.memset.p0.i64(ptr noundef nonnull align 4 dereferenceable(16) %.repack1, i8 0, i64 16, i1 false)
  store <2 x i64> <i64 1, i64 3>, ptr %2, align 4
  %x37 = ptrtoint ptr %2 to i64
  %3 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 0>, ptr %3, align 4
  %.repack8 = getelementptr inbounds [4 x i64], ptr %3, i64 0, i64 2
  store i64 100, ptr %.repack8, align 4
  %x36 = ptrtoint ptr %3 to i64
  %4 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 5>, ptr %4, align 4
  %.repack12 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 2
  store i64 %x37, ptr %.repack12, align 4
  %.repack14 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 3
  store i64 %x36, ptr %.repack14, align 4
  %x35 = ptrtoint ptr %4 to i64
  %5 = tail call fastcc [4 x i64] @DownFromTail.sum(i64 %x38, i64 %x35)
  %x34 = extractvalue [4 x i64] %5, 2
  tail call fastcc void @printf(ptr nonnull @"%d", i64 %x34)
  ret void
}

; Function Attrs: nounwind
define fastcc void @drop(i64 %x145) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x145 to ptr
  %x144 = load i64, ptr %1, align 4
  %cond = icmp eq i64 %x144, 1
  br i1 %cond, label %"1_10", label %default_10

"1_10":                                           ; preds = %0
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x143 = load i64, ptr %2, align 4
  switch i64 %x143, label %default_11 [
    i64 3, label %"CDownFromTail.List.[]_11"
    i64 4, label %"CDownFromTail.List._\E2\88\B7__11"
    i64 0, label %Cnat_11
    i64 1, label %"FAgda.Builtin.Nat._+__11"
    i64 2, label %FAgda.Builtin.Nat._-__11
    i64 5, label %FDownFromTail.downFrom_11
  ]

common.ret:                                       ; preds = %default_10, %Cnat_11, %"CDownFromTail.List.[]_11", %FDownFromTail.downFrom_11, %FAgda.Builtin.Nat._-__11, %"FAgda.Builtin.Nat._+__11", %"CDownFromTail.List._\E2\88\B7__11"
  ret void

"CDownFromTail.List.[]_11":                       ; preds = %"1_10"
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

"CDownFromTail.List._\E2\88\B7__11":              ; preds = %"1_10"
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x136 = load i64, ptr %3, align 4
  tail call fastcc void @drop(i64 %x136)
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x135 = load i64, ptr %4, align 4
  tail call fastcc void @drop(i64 %x135)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

Cnat_11:                                          ; preds = %"1_10"
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

"FAgda.Builtin.Nat._+__11":                       ; preds = %"1_10"
  %5 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x138 = load i64, ptr %5, align 4
  tail call fastcc void @drop(i64 %x138)
  %6 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x137 = load i64, ptr %6, align 4
  tail call fastcc void @drop(i64 %x137)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

FAgda.Builtin.Nat._-__11:                         ; preds = %"1_10"
  %7 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x140 = load i64, ptr %7, align 4
  tail call fastcc void @drop(i64 %x140)
  %8 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x139 = load i64, ptr %8, align 4
  tail call fastcc void @drop(i64 %x139)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

FDownFromTail.downFrom_11:                        ; preds = %"1_10"
  %9 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x142 = load i64, ptr %9, align 4
  tail call fastcc void @drop(i64 %x142)
  %10 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x141 = load i64, ptr %10, align 4
  tail call fastcc void @drop(i64 %x141)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

default_11:                                       ; preds = %"1_10"
  unreachable

default_10:                                       ; preds = %0
  %x134 = add i64 %x144, -1
  store i64 %x134, ptr %1, align 4
  br label %common.ret
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn
define fastcc void @dup(i64 %x133) local_unnamed_addr #4 {
  %1 = inttoptr i64 %x133 to ptr
  %x132 = load i64, ptr %1, align 4
  %x131 = add i64 %x132, 1
  store i64 %x131, ptr %1, align 4
  ret void
}

; Function Attrs: argmemonly nocallback nofree nounwind willreturn writeonly
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #5

attributes #0 = { nofree nounwind }
attributes #1 = { inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) "alloc-family"="malloc" }
attributes #2 = { inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free") "alloc-family"="malloc" }
attributes #3 = { nounwind }
attributes #4 = { mustprogress nofree norecurse nosync nounwind willreturn }
attributes #5 = { argmemonly nocallback nofree nounwind willreturn writeonly }