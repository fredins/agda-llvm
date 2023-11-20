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
  %x65 = load i64, ptr %2, align 4
  %switch = icmp eq i64 %x65, 0
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x82 = load i64, ptr %3, align 4
  br i1 %switch, label %Cnat_0, label %FAgda.Builtin.Nat._-__0

Cnat_0:                                           ; preds = %0
  %Cnat_0_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x82, 2
  br label %continue_0

FAgda.Builtin.Nat._-__0:                          ; preds = %0
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x84 = load i64, ptr %4, align 4
  %FAgda.Builtin.Nat._-__0_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x82, i64 %x84)
  br label %continue_0

continue_0:                                       ; preds = %FAgda.Builtin.Nat._-__0, %Cnat_0
  %5 = phi [4 x i64] [ %Cnat_0_res, %Cnat_0 ], [ %FAgda.Builtin.Nat._-__0_res, %FAgda.Builtin.Nat._-__0 ]
  %x64 = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %2, align 4
  %.repack4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x64, ptr %.repack4, align 4
  %x107 = load i64, ptr %1, align 4
  %cond = icmp eq i64 %x107, 1
  br i1 %cond, label %"1_2", label %default_2

"1_2":                                            ; preds = %continue_0
  tail call fastcc void @free(ptr nonnull %1)
  br label %continue_1

default_2:                                        ; preds = %continue_0
  %x106 = add i64 %x107, -1
  store i64 %x106, ptr %1, align 4
  br label %continue_1

continue_1:                                       ; preds = %default_2, %"1_2"
  %6 = inttoptr i64 %x4 to ptr
  %7 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  %x44 = load i64, ptr %7, align 4
  %.repack8 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 1
  store i64 0, ptr %.repack8, align 4
  %x109 = load i64, ptr %6, align 4
  %cond1 = icmp eq i64 %x109, 1
  br i1 %cond1, label %"1_4", label %default_4

"1_4":                                            ; preds = %continue_1
  tail call fastcc void @free(ptr nonnull %6)
  br label %continue_3

default_4:                                        ; preds = %continue_1
  %x108 = add i64 %x109, -1
  store i64 %x108, ptr %6, align 4
  br label %continue_3

continue_3:                                       ; preds = %default_4, %"1_4"
  %x2 = add i64 %x44, %x64
  %8 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x2, 2
  ret [4 x i64] %8
}

; Function Attrs: nounwind
define fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x9, i64 %x10) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x9 to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x69 = load i64, ptr %2, align 4
  %switch = icmp eq i64 %x69, 0
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x85 = load i64, ptr %3, align 4
  br i1 %switch, label %Cnat_5, label %FAgda.Builtin.Nat._-__5

Cnat_5:                                           ; preds = %0
  %Cnat_5_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x85, 2
  br label %continue_5

FAgda.Builtin.Nat._-__5:                          ; preds = %0
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x87 = load i64, ptr %4, align 4
  %FAgda.Builtin.Nat._-__5_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x85, i64 %x87)
  br label %continue_5

continue_5:                                       ; preds = %FAgda.Builtin.Nat._-__5, %Cnat_5
  %5 = phi [4 x i64] [ %Cnat_5_res, %Cnat_5 ], [ %FAgda.Builtin.Nat._-__5_res, %FAgda.Builtin.Nat._-__5 ]
  %x68 = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %2, align 4
  %.repack4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x68, ptr %.repack4, align 4
  %x111 = load i64, ptr %1, align 4
  %cond = icmp eq i64 %x111, 1
  br i1 %cond, label %"1_7", label %default_7

"1_7":                                            ; preds = %continue_5
  tail call fastcc void @free(ptr nonnull %1)
  br label %continue_6

default_7:                                        ; preds = %continue_5
  %x110 = add i64 %x111, -1
  store i64 %x110, ptr %1, align 4
  br label %continue_6

continue_6:                                       ; preds = %default_7, %"1_7"
  %6 = inttoptr i64 %x10 to ptr
  %7 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  %x51 = load i64, ptr %7, align 4
  %.repack8 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 1
  store i64 0, ptr %.repack8, align 4
  %x113 = load i64, ptr %6, align 4
  %cond1 = icmp eq i64 %x113, 1
  br i1 %cond1, label %"1_9", label %default_9

"1_9":                                            ; preds = %continue_6
  tail call fastcc void @free(ptr nonnull %6)
  br label %continue_8

default_9:                                        ; preds = %continue_6
  %x112 = add i64 %x113, -1
  store i64 %x112, ptr %6, align 4
  br label %continue_8

continue_8:                                       ; preds = %default_9, %"1_9"
  %x8 = sub i64 %x68, %x51
  %8 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x8, 2
  ret [4 x i64] %8
}

; Function Attrs: nounwind
define fastcc [4 x i64] @DownFromOpt.downFrom(i64 %x19) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x19 to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x73 = load i64, ptr %2, align 4
  %switch = icmp eq i64 %x73, 0
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x88 = load i64, ptr %3, align 4
  br i1 %switch, label %Cnat_10, label %FAgda.Builtin.Nat._-__10

Cnat_10:                                          ; preds = %0
  %Cnat_10_res = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x88, 2
  br label %continue_10

FAgda.Builtin.Nat._-__10:                         ; preds = %0
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x90 = load i64, ptr %4, align 4
  %FAgda.Builtin.Nat._-__10_res = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x88, i64 %x90)
  br label %continue_10

continue_10:                                      ; preds = %FAgda.Builtin.Nat._-__10, %Cnat_10
  %5 = phi [4 x i64] [ %Cnat_10_res, %Cnat_10 ], [ %FAgda.Builtin.Nat._-__10_res, %FAgda.Builtin.Nat._-__10 ]
  %x72 = extractvalue [4 x i64] %5, 2
  store i64 0, ptr %2, align 4
  %.repack4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x72, ptr %.repack4, align 4
  %cond = icmp eq i64 %x72, 0
  br i1 %cond, label %"0_11", label %default_11

"0_11":                                           ; preds = %continue_10
  %x115 = load i64, ptr %1, align 4
  %cond1 = icmp eq i64 %x115, 1
  br i1 %cond1, label %"1_13", label %default_13

"1_13":                                           ; preds = %"0_11"
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

default_13:                                       ; preds = %"0_11"
  %x114 = add i64 %x115, -1
  store i64 %x114, ptr %1, align 4
  br label %common.ret

common.ret:                                       ; preds = %"1_13", %default_13, %default_11
  %common.ret.op = phi [4 x i64] [ %10, %default_11 ], [ [i64 undef, i64 2, i64 undef, i64 undef], %default_13 ], [ [i64 undef, i64 2, i64 undef, i64 undef], %"1_13" ]
  ret [4 x i64] %common.ret.op

default_11:                                       ; preds = %continue_10
  %6 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 0>, ptr %6, align 4
  %.repack9 = getelementptr inbounds [4 x i64], ptr %6, i64 0, i64 2
  store i64 1, ptr %.repack9, align 4
  %x17 = ptrtoint ptr %6 to i64
  %7 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  %.repack13 = getelementptr inbounds [4 x i64], ptr %7, i64 0, i64 2
  store i64 %x19, ptr %.repack13, align 4
  %.repack15 = getelementptr inbounds [4 x i64], ptr %7, i64 0, i64 3
  store i64 %x17, ptr %.repack15, align 4
  %x16 = ptrtoint ptr %7 to i64
  store <2 x i64> <i64 2, i64 1>, ptr %7, align 4
  %8 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 3>, ptr %8, align 4
  %.repack19 = getelementptr inbounds [4 x i64], ptr %8, i64 0, i64 2
  store i64 %x16, ptr %.repack19, align 4
  %x15 = ptrtoint ptr %8 to i64
  %9 = insertvalue [4 x i64] [i64 undef, i64 4, i64 undef, i64 undef], i64 %x16, 2
  %10 = insertvalue [4 x i64] %9, i64 %x15, 3
  br label %common.ret
}

; Function Attrs: nounwind
define fastcc [4 x i64] @DownFromOpt.sum(i64 %x27, i64 %x28) local_unnamed_addr #3 {
  br label %tailrecurse

tailrecurse:                                      ; preds = %"Agda.Builtin.Nat._+_.exit", %0
  %x27.tr = phi i64 [ %x27, %0 ], [ %x22, %"Agda.Builtin.Nat._+_.exit" ]
  %x28.tr = phi i64 [ %x28, %0 ], [ %x78, %"Agda.Builtin.Nat._+_.exit" ]
  %1 = inttoptr i64 %x28.tr to ptr
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x79 = load i64, ptr %2, align 4
  switch i64 %x79, label %default_14 [
    i64 2, label %continue_14
    i64 4, label %"CDownFromOpt.List._\E2\88\B7__14"
    i64 3, label %FDownFromOpt.downFrom_14
  ]

"CDownFromOpt.List._\E2\88\B7__14":               ; preds = %tailrecurse
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x91 = load i64, ptr %3, align 4
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x93 = load i64, ptr %4, align 4
  %5 = insertvalue [4 x i64] [i64 undef, i64 4, i64 undef, i64 undef], i64 %x91, 2
  %"CDownFromOpt.List._\E2\88\B7__14_res" = insertvalue [4 x i64] %5, i64 %x93, 3
  br label %continue_14

FDownFromOpt.downFrom_14:                         ; preds = %tailrecurse
  %6 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x92 = load i64, ptr %6, align 4
  %FDownFromOpt.downFrom_14_res = tail call fastcc [4 x i64] @DownFromOpt.downFrom(i64 %x92)
  br label %continue_14

default_14:                                       ; preds = %tailrecurse
  unreachable

continue_14:                                      ; preds = %tailrecurse, %FDownFromOpt.downFrom_14, %"CDownFromOpt.List._\E2\88\B7__14"
  %7 = phi [4 x i64] [ %"CDownFromOpt.List._\E2\88\B7__14_res", %"CDownFromOpt.List._\E2\88\B7__14" ], [ %FDownFromOpt.downFrom_14_res, %FDownFromOpt.downFrom_14 ], [ [i64 undef, i64 2, i64 undef, i64 undef], %tailrecurse ]
  %x76 = extractvalue [4 x i64] %7, 1
  %x77 = extractvalue [4 x i64] %7, 2
  %x78 = extractvalue [4 x i64] %7, 3
  %switch = icmp eq i64 %x76, 2
  br i1 %switch, label %"CDownFromOpt.List.[]_15", label %"CDownFromOpt.List._\E2\88\B7__15"

"CDownFromOpt.List.[]_15":                        ; preds = %continue_14
  %8 = inttoptr i64 %x28.tr to ptr
  store i64 2, ptr %2, align 4
  %x117 = load i64, ptr %8, align 4
  %cond1 = icmp eq i64 %x117, 1
  br i1 %cond1, label %"1_17", label %default_17

"1_17":                                           ; preds = %"CDownFromOpt.List.[]_15"
  tail call fastcc void @free(ptr nonnull %8)
  br label %continue_16

default_17:                                       ; preds = %"CDownFromOpt.List.[]_15"
  %x116 = add i64 %x117, -1
  store i64 %x116, ptr %8, align 4
  br label %continue_16

continue_16:                                      ; preds = %default_17, %"1_17"
  %9 = inttoptr i64 %x27.tr to ptr
  %10 = getelementptr inbounds [4 x i64], ptr %9, i64 0, i64 2
  %x63 = load i64, ptr %10, align 4
  %.repack21 = getelementptr inbounds [4 x i64], ptr %9, i64 0, i64 1
  store i64 0, ptr %.repack21, align 4
  %x119 = load i64, ptr %9, align 4
  %cond2 = icmp eq i64 %x119, 1
  br i1 %cond2, label %"1_19", label %default_19

"1_19":                                           ; preds = %continue_16
  tail call fastcc void @free(ptr nonnull %9)
  br label %continue_18

default_19:                                       ; preds = %continue_16
  %x118 = add i64 %x119, -1
  store i64 %x118, ptr %9, align 4
  br label %continue_18

continue_18:                                      ; preds = %default_19, %"1_19"
  %11 = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x63, 2
  ret [4 x i64] %11

"CDownFromOpt.List._\E2\88\B7__15":               ; preds = %continue_14
  store i64 4, ptr %2, align 4
  %.repack5 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  store i64 %x77, ptr %.repack5, align 4
  %.repack7 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  store i64 %x78, ptr %.repack7, align 4
  %x121 = load i64, ptr %1, align 4
  %cond = icmp eq i64 %x121, 1
  br i1 %cond, label %"1_21", label %default_21

"1_21":                                           ; preds = %"CDownFromOpt.List._\E2\88\B7__15"
  tail call fastcc void @free(ptr nonnull %1)
  %.pre = inttoptr i64 %x77 to ptr
  br label %continue_20

default_21:                                       ; preds = %"CDownFromOpt.List._\E2\88\B7__15"
  %12 = inttoptr i64 %x78 to ptr
  %x104.i = load i64, ptr %12, align 4
  %x103.i = add i64 %x104.i, 1
  store i64 %x103.i, ptr %12, align 4
  %13 = inttoptr i64 %x77 to ptr
  %x104.i27 = load i64, ptr %13, align 4
  %x103.i28 = add i64 %x104.i27, 1
  store i64 %x103.i28, ptr %13, align 4
  %x120 = add i64 %x121, -1
  store i64 %x120, ptr %1, align 4
  br label %continue_20

continue_20:                                      ; preds = %default_21, %"1_21"
  %.pre-phi = phi ptr [ %13, %default_21 ], [ %.pre, %"1_21" ]
  %14 = getelementptr inbounds [4 x i64], ptr %.pre-phi, i64 0, i64 1
  %x65.i = load i64, ptr %14, align 4
  %switch.i = icmp eq i64 %x65.i, 0
  %15 = getelementptr inbounds [4 x i64], ptr %.pre-phi, i64 0, i64 2
  %x82.i = load i64, ptr %15, align 4
  br i1 %switch.i, label %Cnat_0.i, label %FAgda.Builtin.Nat._-__0.i

Cnat_0.i:                                         ; preds = %continue_20
  %Cnat_0_res.i = insertvalue [4 x i64] [i64 undef, i64 0, i64 undef, i64 undef], i64 %x82.i, 2
  br label %continue_0.i

FAgda.Builtin.Nat._-__0.i:                        ; preds = %continue_20
  %16 = getelementptr inbounds [4 x i64], ptr %.pre-phi, i64 0, i64 3
  %x84.i = load i64, ptr %16, align 4
  %FAgda.Builtin.Nat._-__0_res.i = tail call fastcc [4 x i64] @Agda.Builtin.Nat._-_(i64 %x82.i, i64 %x84.i)
  br label %continue_0.i

continue_0.i:                                     ; preds = %FAgda.Builtin.Nat._-__0.i, %Cnat_0.i
  %17 = phi [4 x i64] [ %Cnat_0_res.i, %Cnat_0.i ], [ %FAgda.Builtin.Nat._-__0_res.i, %FAgda.Builtin.Nat._-__0.i ]
  %x64.i = extractvalue [4 x i64] %17, 2
  store i64 0, ptr %14, align 4
  store i64 %x64.i, ptr %15, align 4
  %x107.i = load i64, ptr %.pre-phi, align 4
  %cond.i = icmp eq i64 %x107.i, 1
  br i1 %cond.i, label %"1_2.i", label %default_2.i

"1_2.i":                                          ; preds = %continue_0.i
  tail call fastcc void @free(ptr nonnull %.pre-phi)
  br label %continue_1.i

default_2.i:                                      ; preds = %continue_0.i
  %x106.i = add i64 %x107.i, -1
  store i64 %x106.i, ptr %.pre-phi, align 4
  br label %continue_1.i

continue_1.i:                                     ; preds = %default_2.i, %"1_2.i"
  %18 = inttoptr i64 %x27.tr to ptr
  %19 = getelementptr inbounds [4 x i64], ptr %18, i64 0, i64 2
  %x44.i = load i64, ptr %19, align 4
  %.repack8.i = getelementptr inbounds [4 x i64], ptr %18, i64 0, i64 1
  store i64 0, ptr %.repack8.i, align 4
  %x109.i = load i64, ptr %18, align 4
  %cond1.i = icmp eq i64 %x109.i, 1
  br i1 %cond1.i, label %"1_4.i", label %default_4.i

"1_4.i":                                          ; preds = %continue_1.i
  tail call fastcc void @free(ptr nonnull %18)
  br label %"Agda.Builtin.Nat._+_.exit"

default_4.i:                                      ; preds = %continue_1.i
  %x108.i = add i64 %x109.i, -1
  store i64 %x108.i, ptr %18, align 4
  br label %"Agda.Builtin.Nat._+_.exit"

"Agda.Builtin.Nat._+_.exit":                      ; preds = %"1_4.i", %default_4.i
  %x2.i = add i64 %x44.i, %x64.i
  %20 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 0>, ptr %20, align 4
  %.repack11 = getelementptr inbounds [4 x i64], ptr %20, i64 0, i64 2
  store i64 %x2.i, ptr %.repack11, align 4
  %x22 = ptrtoint ptr %20 to i64
  br label %tailrecurse
}

define fastcc void @main() local_unnamed_addr {
  %1 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store i64 1, ptr %1, align 4
  %.repack1 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x36 = ptrtoint ptr %1 to i64
  %2 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  tail call void @llvm.memset.p0.i64(ptr noundef nonnull align 4 dereferenceable(16) %.repack1, i8 0, i64 16, i1 false)
  store <2 x i64> <i64 1, i64 0>, ptr %2, align 4
  %.repack5 = getelementptr inbounds [4 x i64], ptr %2, i64 0, i64 2
  store i64 100, ptr %.repack5, align 4
  %x35 = ptrtoint ptr %2 to i64
  %3 = tail call fastcc dereferenceable_or_null(32) ptr @malloc(i64 32)
  store <2 x i64> <i64 1, i64 3>, ptr %3, align 4
  %.repack9 = getelementptr inbounds [4 x i64], ptr %3, i64 0, i64 2
  store i64 %x35, ptr %.repack9, align 4
  %x34 = ptrtoint ptr %3 to i64
  %4 = tail call fastcc [4 x i64] @DownFromOpt.sum(i64 %x36, i64 %x34)
  %x33 = extractvalue [4 x i64] %4, 2
  tail call fastcc void @printf(ptr nonnull @"%d", i64 %x33)
  ret void
}

; Function Attrs: nounwind
define fastcc void @drop(i64 %x102) local_unnamed_addr #3 {
  %1 = inttoptr i64 %x102 to ptr
  %x101 = load i64, ptr %1, align 4
  %cond = icmp eq i64 %x101, 1
  br i1 %cond, label %"1_22", label %default_22

"1_22":                                           ; preds = %0
  %2 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 1
  %x100 = load i64, ptr %2, align 4
  switch i64 %x100, label %default_23 [
    i64 2, label %"CDownFromOpt.List.[]_23"
    i64 4, label %"CDownFromOpt.List._\E2\88\B7__23"
    i64 0, label %Cnat_23
    i64 1, label %FAgda.Builtin.Nat._-__23
    i64 3, label %FDownFromOpt.downFrom_23
  ]

common.ret:                                       ; preds = %default_22, %Cnat_23, %"CDownFromOpt.List.[]_23", %FDownFromOpt.downFrom_23, %FAgda.Builtin.Nat._-__23, %"CDownFromOpt.List._\E2\88\B7__23"
  ret void

"CDownFromOpt.List.[]_23":                        ; preds = %"1_22"
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

"CDownFromOpt.List._\E2\88\B7__23":               ; preds = %"1_22"
  %3 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x96 = load i64, ptr %3, align 4
  tail call fastcc void @drop(i64 %x96)
  %4 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x95 = load i64, ptr %4, align 4
  tail call fastcc void @drop(i64 %x95)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

Cnat_23:                                          ; preds = %"1_22"
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

FAgda.Builtin.Nat._-__23:                         ; preds = %"1_22"
  %5 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x98 = load i64, ptr %5, align 4
  tail call fastcc void @drop(i64 %x98)
  %6 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 3
  %x97 = load i64, ptr %6, align 4
  tail call fastcc void @drop(i64 %x97)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

FDownFromOpt.downFrom_23:                         ; preds = %"1_22"
  %7 = getelementptr inbounds [4 x i64], ptr %1, i64 0, i64 2
  %x99 = load i64, ptr %7, align 4
  tail call fastcc void @drop(i64 %x99)
  tail call fastcc void @free(ptr nonnull %1)
  br label %common.ret

default_23:                                       ; preds = %"1_22"
  unreachable

default_22:                                       ; preds = %0
  %x94 = add i64 %x101, -1
  store i64 %x94, ptr %1, align 4
  br label %common.ret
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn
define fastcc void @dup(i64 %x105) local_unnamed_addr #4 {
  %1 = inttoptr i64 %x105 to ptr
  %x104 = load i64, ptr %1, align 4
  %x103 = add i64 %x104, 1
  store i64 %x103, ptr %1, align 4
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
