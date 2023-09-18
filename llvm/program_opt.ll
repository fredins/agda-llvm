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
define fastcc [4 x i64] @DownFrom.downFrom(i64 %x7) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x7 to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x56 = load i64, ptr %2, align 4
  %switch = icmp eq i64 %x56, 0
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x72 = load i64, ptr %3, align 4
  br i1 %switch, label %Cnat_0, label %FAgda.Builtin.Nat._-__0

Cnat_0:                                           ; preds = %0
  %Cnat_0_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x72, 2
  br label %continue_0

FAgda.Builtin.Nat._-__0:                          ; preds = %0
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x74 = load i64, ptr %4, align 4
  %FAgda.Builtin.Nat._-__0_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x72, i64 %x74)
  br label %continue_0

continue_0:                                       ; preds = %FAgda.Builtin.Nat._-__0, %Cnat_0
  %5 = phi [4 x i64] [ %Cnat_0_res, %Cnat_0 ], [ %FAgda.Builtin.Nat._-__0_res, %FAgda.Builtin.Nat._-__0 ]
  %x55 = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %2, align 4
  %.repack3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x55, ptr %.repack3, align 4
  %cond = icmp eq i64 %x55, 0
  br i1 %cond, label %"0_1", label %default_1

common.ret:                                       ; preds = %default_1, %"0_1"
  %common.ret.op = phi [4 x i64] [ [i64 undef, i64 2, i64 undef, i64 undef], %"0_1" ], [ %10, %default_1 ]
  ret [4 x i64] %common.ret.op

"0_1":                                            ; preds = %continue_0
  tail call fastcc void @drop(i64 %x7)
  br label %common.ret

default_1:                                        ; preds = %continue_0
  %6 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 0>, ptr %6, align 4
  %.repack8 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  store i64 1, ptr %.repack8, align 4
  %x5 = ptrtoint ptr %6 to i64
  %7 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  %.repack12 = getelementptr inbounds [4 x i64], ptr %7, i64 0, i64 2
  store i64 %x7, ptr %.repack12, align 4
  %.repack14 = getelementptr inbounds [4 x i64], ptr %7, i64 0, i64 3
  store i64 %x5, ptr %.repack14, align 4
  %x4 = ptrtoint ptr %7 to i64
  %8 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  %.repack18 = getelementptr inbounds [4 x i64], ptr %8, i64 0, i64 2
  store i64 %x4, ptr %.repack18, align 4
  %x3 = ptrtoint ptr %8 to i64
  store <2 x i64> <i64 2, i64 3>, ptr %8, align 4
  store <2 x i64> <i64 3, i64 1>, ptr %7, align 4
  %9 = insertvalue [4 x i64] [i64 undef, i64 4, i64 undef, i64 undef], i64 %x4, 2
  %10 = insertvalue [4 x i64] %9, i64 %x3, 3
  br label %common.ret
}

; Function Attrs: nounwind
define fastcc [4 x i64] @DownFrom.sum(i64 %x14) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x14 to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x40 = load i64, ptr %2, align 4
  %3 = tail call fastcc [4 x i64] @DownFrom.downFrom(i64 %x40)
  %x59 = extractvalue [4 x i64] %3, 1
  %switch = icmp eq i64 %x59, 2
  br i1 %switch, label %"CDownFrom.List.[]_2", label %"CDownFrom.List._\E2\88\B7__2"

common.ret:                                       ; preds = %"Agda.Builtin.Nat._+_.exit", %"CDownFrom.List.[]_2"
  %common.ret.op = phi [4 x i64] [ [i64 undef, i64 0, i64 0, i64 undef], %"CDownFrom.List.[]_2" ], [ %11, %"Agda.Builtin.Nat._+_.exit" ]
  ret [4 x i64] %common.ret.op

"CDownFrom.List.[]_2":                            ; preds = %0
  %.repack13 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  store i64 2, ptr %.repack13, align 4
  %.elt16 = extractvalue [4 x i64] %3, 2
  store i64 %.elt16, ptr %2, align 4
  %.repack17 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %.elt18 = extractvalue [4 x i64] %3, 3
  store i64 %.elt18, ptr %.repack17, align 4
  tail call fastcc void @drop(i64 %x14)
  br label %common.ret

"CDownFrom.List._\E2\88\B7__2":                   ; preds = %0
  %x61 = extractvalue [4 x i64] %3, 3
  %x60 = extractvalue [4 x i64] %3, 2
  %.repack1 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  store i64 %x59, ptr %.repack1, align 4
  store i64 %x60, ptr %2, align 4
  %.repack5 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  store i64 %x61, ptr %.repack5, align 4
  tail call fastcc void @drop(i64 %x14)
  %4 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store i64 1, ptr %4, align 4
  %.repack7 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 1
  store i64 5, ptr %.repack7, align 4
  %.repack9 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 2
  store i64 %x61, ptr %.repack9, align 4
  %x10 = ptrtoint ptr %4 to i64
  %5 = tail call fastcc [4 x i64] @DownFrom.sum(i64 %x61)
  %x66.i = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %.repack7, align 4
  store i64 %x66.i, ptr %.repack9, align 4
  tail call fastcc void @drop(i64 %x10)
  %6 = inttoptr i64 %x60 to ptr
  %7 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 1
  %x63.i = load i64, ptr %7, align 4
  %switch.i = icmp eq i64 %x63.i, 0
  %8 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  %x75.i = load i64, ptr %8, align 4
  br i1 %switch.i, label %Cnat_3.i, label %FAgda.Builtin.Nat._-__3.i

Cnat_3.i:                                         ; preds = %"CDownFrom.List._\E2\88\B7__2"
  %Cnat_3_res.i = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x75.i, 2
  br label %"Agda.Builtin.Nat._+_.exit"

FAgda.Builtin.Nat._-__3.i:                        ; preds = %"CDownFrom.List._\E2\88\B7__2"
  %9 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 3
  %x77.i = load i64, ptr %9, align 4
  %FAgda.Builtin.Nat._-__3_res.i = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x75.i, i64 %x77.i)
  br label %"Agda.Builtin.Nat._+_.exit"

"Agda.Builtin.Nat._+_.exit":                      ; preds = %Cnat_3.i, %FAgda.Builtin.Nat._-__3.i
  %10 = phi [4 x i64] [ %Cnat_3_res.i, %Cnat_3.i ], [ %FAgda.Builtin.Nat._-__3_res.i, %FAgda.Builtin.Nat._-__3.i ]
  %x62.i = extractvalue [4 x i64] %10, 2
  store i64 0, ptr %7, align 4
  %.repack9.i = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  store i64 %x62.i, ptr %.repack9.i, align 4
  tail call fastcc void @drop(i64 %x60)
  %x24.i = add i64 %x62.i, %x66.i
  %11 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x24.i, 2
  br label %common.ret
}

define fastcc void @main() local_unnamed_addr {
  %1 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 0>, ptr %1, align 4
  %.repack2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 100, ptr %.repack2, align 4
  %x20 = ptrtoint ptr %1 to i64
  %2 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 3>, ptr %2, align 4
  %.repack6 = getelementptr inbounds [4 x i64], ptr %2, i64 0, i64 2
  store i64 %x20, ptr %.repack6, align 4
  %x19 = ptrtoint ptr %2 to i64
  %3 = tail call fastcc [4 x i64] @DownFrom.sum(i64 %x19)
  %x18 = extractvalue [4 x i64] %3, 2
  tail call fastcc void @printf(ptr nonnull @"%d", i64 %x18)
  ret void
}

; Function Attrs: nounwind
define fastcc [4 x i64] @"Agda.Builtin.Nat._+_"(i64 %x25, i64 %x26) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x25 to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x42 = load i64, ptr %2, align 4
  %3 = tail call fastcc [4 x i64] @DownFrom.sum(i64 %x42)
  %x66 = extractvalue [4 x i64] %3, 2
  %.repack1 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  store i64 0, ptr %.repack1, align 4
  store i64 %x66, ptr %2, align 4
  tail call fastcc void @drop(i64 %x25)
  %4 = inttoptr i64 %x26 to ptr
  %5 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 1
  %x63 = load i64, ptr %5, align 4
  %switch = icmp eq i64 %x63, 0
  %6 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 2
  %x75 = load i64, ptr %6, align 4
  br i1 %switch, label %Cnat_3, label %FAgda.Builtin.Nat._-__3

Cnat_3:                                           ; preds = %0
  %Cnat_3_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x75, 2
  br label %continue_3

FAgda.Builtin.Nat._-__3:                          ; preds = %0
  %7 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 3
  %x77 = load i64, ptr %7, align 4
  %FAgda.Builtin.Nat._-__3_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x75, i64 %x77)
  br label %continue_3

continue_3:                                       ; preds = %FAgda.Builtin.Nat._-__3, %Cnat_3
  %8 = phi [4 x i64] [ %Cnat_3_res, %Cnat_3 ], [ %FAgda.Builtin.Nat._-__3_res, %FAgda.Builtin.Nat._-__3 ]
  %x62 = extractvalue [4 x i64] %8, 2
  store i64 0, ptr %5, align 4
  %.repack9 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 2
  store i64 %x62, ptr %.repack9, align 4
  tail call fastcc void @drop(i64 %x26)
  %x24 = add i64 %x62, %x66
  %9 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x24, 2
  ret [4 x i64] %9
}

; Function Attrs: nounwind
define fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x31, i64 %x32) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x31 to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x69 = load i64, ptr %2, align 4
  %switch = icmp eq i64 %x69, 0
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x78 = load i64, ptr %3, align 4
  br i1 %switch, label %Cnat_4, label %FAgda.Builtin.Nat._-__4

Cnat_4:                                           ; preds = %0
  %Cnat_4_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x78, 2
  br label %continue_4

FAgda.Builtin.Nat._-__4:                          ; preds = %0
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x80 = load i64, ptr %4, align 4
  %FAgda.Builtin.Nat._-__4_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x78, i64 %x80)
  br label %continue_4

continue_4:                                       ; preds = %FAgda.Builtin.Nat._-__4, %Cnat_4
  %5 = phi [4 x i64] [ %Cnat_4_res, %Cnat_4 ], [ %FAgda.Builtin.Nat._-__4_res, %FAgda.Builtin.Nat._-__4 ]
  %x68 = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %2, align 4
  %.repack3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x68, ptr %.repack3, align 4
  tail call fastcc void @drop(i64 %x31)
  %6 = inttoptr i64 %x32 to ptr
  %7 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  %x54 = load i64, ptr %7, align 4
  %.repack7 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 1
  store i64 0, ptr %.repack7, align 4
  tail call fastcc void @drop(i64 %x32)
  %x30 = sub i64 %x68, %x54
  %8 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x30, 2
  ret [4 x i64] %8
}

; Function Attrs: nounwind
define fastcc void @drop(i64 %x93) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x93 to ptr
  %x92 = load i64, ptr %1, align 4
  %cond = icmp eq i64 %x92, 1
  br i1 %cond, label %"1_5", label %default_5

"1_5":                                            ; preds = %0
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x91 = load i64, ptr %2, align 4
  switch i64 %x91, label %default_6 [
    i64 2, label %"CDownFrom.List.[]_6"
    i64 4, label %"CDownFrom.List._\E2\88\B7__6"
    i64 0, label %Cnat_6
    i64 1, label %FAgda.Builtin.Nat._-__6
    i64 3, label %FDownFrom.downFrom_6
    i64 5, label %FDownFrom.sum_6
  ]

common.ret:                                       ; preds = %default_5, %Cnat_6, %"CDownFrom.List.[]_6", %FDownFrom.sum_6, %FDownFrom.downFrom_6, %FAgda.Builtin.Nat._-__6, %"CDownFrom.List._\E2\88\B7__6"
  ret void

"CDownFrom.List.[]_6":                            ; preds = %"1_5"
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

"CDownFrom.List._\E2\88\B7__6":                   ; preds = %"1_5"
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x86 = load i64, ptr %3, align 4
  tail call fastcc void @drop(i64 %x86)
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x85 = load i64, ptr %4, align 4
  tail call fastcc void @drop(i64 %x85)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

Cnat_6:                                           ; preds = %"1_5"
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

FAgda.Builtin.Nat._-__6:                          ; preds = %"1_5"
  %5 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x88 = load i64, ptr %5, align 4
  tail call fastcc void @drop(i64 %x88)
  %6 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x87 = load i64, ptr %6, align 4
  tail call fastcc void @drop(i64 %x87)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

FDownFrom.downFrom_6:                             ; preds = %"1_5"
  %7 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x89 = load i64, ptr %7, align 4
  tail call fastcc void @drop(i64 %x89)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

FDownFrom.sum_6:                                  ; preds = %"1_5"
  %8 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x90 = load i64, ptr %8, align 4
  tail call fastcc void @drop(i64 %x90)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

default_6:                                        ; preds = %"1_5"
  unreachable

default_5:                                        ; preds = %0
  %x84 = add i64 %x92, -1
  store i64 %x84, ptr %1, align 4
  br label %common.ret
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn
define fastcc void @dup(i64 %x83) local_unnamed_addr #4 {
  %1 = inttoptr i64 %x83 to ptr
  %x82 = load i64, ptr %1, align 4
  %x81 = add i64 %x82, 1
  store i64 %x81, ptr %1, align 4
  ret void
}

attributes #0 = { nofree nounwind }
attributes #1 = { inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) "alloc-family"="malloc" }
attributes #2 = { inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free") "alloc-family"="malloc" }
attributes #3 = { nounwind }
attributes #4 = { mustprogress nofree norecurse nosync nounwind willreturn }
