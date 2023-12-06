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
  %x71 = load i64, ptr %3, align 4
  br i1 %switch, label %Cnat_0, label %FAgda.Builtin.Nat._-__0

Cnat_0:                                           ; preds = %0
  %Cnat_0_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x71, 2
  br label %continue_0

FAgda.Builtin.Nat._-__0:                          ; preds = %0
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x73 = load i64, ptr %4, align 4
  %FAgda.Builtin.Nat._-__0_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x71, i64 %x73)
  br label %continue_0

continue_0:                                       ; preds = %FAgda.Builtin.Nat._-__0, %Cnat_0
  %5 = phi [4 x i64] [ %Cnat_0_res, %Cnat_0 ], [ %FAgda.Builtin.Nat._-__0_res, %FAgda.Builtin.Nat._-__0 ]
  %x55 = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %2, align 4
  %.repack4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x55, ptr %.repack4, align 4
  %cond = icmp eq i64 %x55, 0
  br i1 %cond, label %"0_1", label %default_1

"0_1":                                            ; preds = %continue_0
  %x84 = load i64, ptr %1, align 4
  %cond1 = icmp eq i64 %x84, 1
  br i1 %cond1, label %"1_3", label %default_3

"1_3":                                            ; preds = %"0_1"
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

default_3:                                        ; preds = %"0_1"
  %x83 = add i64 %x84, -1
  store i64 %x83, ptr %1, align 4
  br label %common.ret

common.ret:                                       ; preds = %"1_3", %default_3, %default_1
  %common.ret.op = phi [4 x i64] [ %10, %default_1 ], [ [i64 undef, i64 2, i64 undef, i64 undef], %default_3 ], [ [i64 undef, i64 2, i64 undef, i64 undef], %"1_3" ]
  ret [4 x i64] %common.ret.op

default_1:                                        ; preds = %continue_0
  %6 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 0>, ptr %6, align 4
  %.repack9 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  store i64 1, ptr %.repack9, align 4
  %x5 = ptrtoint ptr %6 to i64
  %7 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  %.repack13 = getelementptr inbounds [4 x i64], ptr %7, i64 0, i64 2
  store i64 %x7, ptr %.repack13, align 4
  %.repack15 = getelementptr inbounds [4 x i64], ptr %7, i64 0, i64 3
  store i64 %x5, ptr %.repack15, align 4
  %x4 = ptrtoint ptr %7 to i64
  store <2 x i64> <i64 2, i64 1>, ptr %7, align 4
  %8 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 3>, ptr %8, align 4
  %.repack19 = getelementptr inbounds [4 x i64], ptr %8, i64 0, i64 2
  store i64 %x4, ptr %.repack19, align 4
  %x3 = ptrtoint ptr %8 to i64
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
  %x60 = extractvalue [4 x i64] %3, 2
  %x61 = extractvalue [4 x i64] %3, 3
  %switch = icmp eq i64 %x59, 2
  %.repack14 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  br i1 %switch, label %"CDownFrom.List.[]_4", label %"CDownFrom.List._\E2\88\B7__4"

"CDownFrom.List.[]_4":                            ; preds = %0
  store i64 2, ptr %.repack14, align 4
  %x86 = load i64, ptr %1, align 4
  %cond1 = icmp eq i64 %x86, 1
  br i1 %cond1, label %"1_6", label %default_6

"1_6":                                            ; preds = %"CDownFrom.List.[]_4"
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

default_6:                                        ; preds = %"CDownFrom.List.[]_4"
  %x85 = add i64 %x86, -1
  store i64 %x85, ptr %1, align 4
  br label %common.ret

common.ret:                                       ; preds = %"1_6", %default_6, %"Agda.Builtin.Nat._+_.exit"
  %common.ret.op = phi [4 x i64] [ %11, %"Agda.Builtin.Nat._+_.exit" ], [ [i64 undef, i64 0, i64 0, i64 undef], %default_6 ], [ [i64 undef, i64 0, i64 0, i64 undef], %"1_6" ]
  ret [4 x i64] %common.ret.op

"CDownFrom.List._\E2\88\B7__4":                   ; preds = %0
  store i64 4, ptr %.repack14, align 4
  store i64 %x60, ptr %2, align 4
  %.repack6 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  store i64 %x61, ptr %.repack6, align 4
  %x88 = load i64, ptr %1, align 4
  %cond = icmp eq i64 %x88, 1
  br i1 %cond, label %"1_8", label %default_8

"1_8":                                            ; preds = %"CDownFrom.List._\E2\88\B7__4"
  tail call fastcc void @free(ptr nonnull %1)
  %.pre = inttoptr i64 %x60 to ptr
  br label %continue_9.i

default_8:                                        ; preds = %"CDownFrom.List._\E2\88\B7__4"
  %4 = inttoptr i64 %x61 to ptr
  %x81.i = load i64, ptr %4, align 4
  %x80.i = add i64 %x81.i, 1
  store i64 %x80.i, ptr %4, align 4
  %5 = inttoptr i64 %x60 to ptr
  %x81.i20 = load i64, ptr %5, align 4
  %x80.i21 = add i64 %x81.i20, 1
  store i64 %x80.i21, ptr %5, align 4
  %x87 = add i64 %x88, -1
  store i64 %x87, ptr %1, align 4
  br label %continue_9.i

continue_9.i:                                     ; preds = %default_8, %"1_8"
  %.pre-phi = phi ptr [ %5, %default_8 ], [ %.pre, %"1_8" ]
  %6 = tail call fastcc [4 x i64] @DownFrom.sum(i64 %x61)
  %x66.i = extractvalue [4 x i64] %6, 2
  %7 = getelementptr inbounds [4 x i64], ptr %.pre-phi, i64 0, i64 1
  %x63.i = load i64, ptr %7, align 4
  %switch.i = icmp eq i64 %x63.i, 0
  %8 = getelementptr inbounds [4 x i64], ptr %.pre-phi, i64 0, i64 2
  %x74.i = load i64, ptr %8, align 4
  br i1 %switch.i, label %Cnat_11.i, label %FAgda.Builtin.Nat._-__11.i

Cnat_11.i:                                        ; preds = %continue_9.i
  %Cnat_11_res.i = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x74.i, 2
  br label %continue_11.i

FAgda.Builtin.Nat._-__11.i:                       ; preds = %continue_9.i
  %9 = getelementptr inbounds [4 x i64], ptr %.pre-phi, i64 0, i64 3
  %x76.i = load i64, ptr %9, align 4
  %FAgda.Builtin.Nat._-__11_res.i = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x74.i, i64 %x76.i)
  br label %continue_11.i

continue_11.i:                                    ; preds = %FAgda.Builtin.Nat._-__11.i, %Cnat_11.i
  %10 = phi [4 x i64] [ %Cnat_11_res.i, %Cnat_11.i ], [ %FAgda.Builtin.Nat._-__11_res.i, %FAgda.Builtin.Nat._-__11.i ]
  %x62.i = extractvalue [4 x i64] %10, 2
  store i64 0, ptr %7, align 4
  %.repack10.i = getelementptr inbounds [4 x i64], ptr %.pre-phi, i64 0, i64 2
  store i64 %x62.i, ptr %.repack10.i, align 4
  %x92.i = load i64, ptr %.pre-phi, align 4
  %cond1.i = icmp eq i64 %x92.i, 1
  br i1 %cond1.i, label %"1_13.i", label %default_13.i

"1_13.i":                                         ; preds = %continue_11.i
  tail call fastcc void @free(ptr nonnull %.pre-phi)
  br label %"Agda.Builtin.Nat._+_.exit"

default_13.i:                                     ; preds = %continue_11.i
  %x91.i = add i64 %x92.i, -1
  store i64 %x91.i, ptr %.pre-phi, align 4
  br label %"Agda.Builtin.Nat._+_.exit"

"Agda.Builtin.Nat._+_.exit":                      ; preds = %"1_13.i", %default_13.i
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
  %.repack2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  store i64 0, ptr %.repack2, align 4
  store i64 %x66, ptr %2, align 4
  %x90 = load i64, ptr %1, align 4
  %cond = icmp eq i64 %x90, 1
  br i1 %cond, label %"1_10", label %default_10

"1_10":                                           ; preds = %0
  tail call fastcc void @free(ptr nonnull %1)
  br label %continue_9

default_10:                                       ; preds = %0
  %x89 = add i64 %x90, -1
  store i64 %x89, ptr %1, align 4
  br label %continue_9

continue_9:                                       ; preds = %default_10, %"1_10"
  %4 = inttoptr i64 %x26 to ptr
  %5 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 1
  %x63 = load i64, ptr %5, align 4
  %switch = icmp eq i64 %x63, 0
  %6 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 2
  %x74 = load i64, ptr %6, align 4
  br i1 %switch, label %Cnat_11, label %FAgda.Builtin.Nat._-__11

Cnat_11:                                          ; preds = %continue_9
  %Cnat_11_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x74, 2
  br label %continue_11

FAgda.Builtin.Nat._-__11:                         ; preds = %continue_9
  %7 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 3
  %x76 = load i64, ptr %7, align 4
  %FAgda.Builtin.Nat._-__11_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x74, i64 %x76)
  br label %continue_11

continue_11:                                      ; preds = %FAgda.Builtin.Nat._-__11, %Cnat_11
  %8 = phi [4 x i64] [ %Cnat_11_res, %Cnat_11 ], [ %FAgda.Builtin.Nat._-__11_res, %FAgda.Builtin.Nat._-__11 ]
  %x62 = extractvalue [4 x i64] %8, 2
  store i64 0, ptr %5, align 4
  %.repack10 = getelementptr inbounds [4 x i64], ptr %4, i64 0, i64 2
  store i64 %x62, ptr %.repack10, align 4
  %x92 = load i64, ptr %4, align 4
  %cond1 = icmp eq i64 %x92, 1
  br i1 %cond1, label %"1_13", label %default_13

"1_13":                                           ; preds = %continue_11
  tail call fastcc void @free(ptr nonnull %4)
  br label %continue_12

default_13:                                       ; preds = %continue_11
  %x91 = add i64 %x92, -1
  store i64 %x91, ptr %4, align 4
  br label %continue_12

continue_12:                                      ; preds = %default_13, %"1_13"
  %x24 = add i64 %x62, %x66
  %9 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x24, 2
  ret [4 x i64] %9
}

; Function Attrs: nounwind
define fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x31, i64 %x32) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x31 to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x68 = load i64, ptr %2, align 4
  %switch = icmp eq i64 %x68, 0
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x77 = load i64, ptr %3, align 4
  br i1 %switch, label %Cnat_14, label %FAgda.Builtin.Nat._-__14

Cnat_14:                                          ; preds = %0
  %Cnat_14_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x77, 2
  br label %continue_14

FAgda.Builtin.Nat._-__14:                         ; preds = %0
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x79 = load i64, ptr %4, align 4
  %FAgda.Builtin.Nat._-__14_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x77, i64 %x79)
  br label %continue_14

continue_14:                                      ; preds = %FAgda.Builtin.Nat._-__14, %Cnat_14
  %5 = phi [4 x i64] [ %Cnat_14_res, %Cnat_14 ], [ %FAgda.Builtin.Nat._-__14_res, %FAgda.Builtin.Nat._-__14 ]
  %x67 = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %2, align 4
  %.repack4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x67, ptr %.repack4, align 4
  %x94 = load i64, ptr %1, align 4
  %cond = icmp eq i64 %x94, 1
  br i1 %cond, label %"1_16", label %default_16

"1_16":                                           ; preds = %continue_14
  tail call fastcc void @free(ptr nonnull %1)
  br label %continue_15

default_16:                                       ; preds = %continue_14
  %x93 = add i64 %x94, -1
  store i64 %x93, ptr %1, align 4
  br label %continue_15

continue_15:                                      ; preds = %default_16, %"1_16"
  %6 = inttoptr i64 %x32 to ptr
  %7 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  %x54 = load i64, ptr %7, align 4
  %.repack8 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 1
  store i64 0, ptr %.repack8, align 4
  %x96 = load i64, ptr %6, align 4
  %cond1 = icmp eq i64 %x96, 1
  br i1 %cond1, label %"1_18", label %default_18

"1_18":                                           ; preds = %continue_15
  tail call fastcc void @free(ptr nonnull %6)
  br label %continue_17

default_18:                                       ; preds = %continue_15
  %x95 = add i64 %x96, -1
  store i64 %x95, ptr %6, align 4
  br label %continue_17

continue_17:                                      ; preds = %default_18, %"1_18"
  %x30 = sub i64 %x67, %x54
  %8 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x30, 2
  ret [4 x i64] %8
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn
define fastcc void @dup(i64 %x82) local_unnamed_addr #4 {
  %1 = inttoptr i64 %x82 to ptr
  %x81 = load i64, ptr %1, align 4
  %x80 = add i64 %x81, 1
  store i64 %x80, ptr %1, align 4
  ret void
}

attributes #0 = { nofree nounwind }
attributes #1 = { inaccessiblememonly mustprogress nofree nounwind willreturn allockind("alloc,uninitialized") allocsize(0) "alloc-family"="malloc" }
attributes #2 = { inaccessiblemem_or_argmemonly mustprogress nounwind willreturn allockind("free") "alloc-family"="malloc" }
attributes #3 = { nounwind }
attributes #4 = { mustprogress nofree norecurse nosync nounwind willreturn }
