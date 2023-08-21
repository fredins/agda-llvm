target triple = "x86_64-pc-linux-gnu"
target datalayout = "p:64:64:64"
declare void @printf(ptr, ...)
declare ptr @malloc(i64)
%Node = type [4 x i64]
@"%d" = private constant [4 x i8] c"%d\0A\00", align 1
define fastcc %Node
@"DownFrom.downFrom"(i64 %x8){
  %_1 = inttoptr i64 %x8 to ptr
  %_2 = getelementptr inbounds %Node, ptr %_1, i32 0
  %x1652 = load i64, ptr %_2
  switch i64 %x1652, label %alt_default [ i64 0, label %alt_0
                                          i64 1, label %alt_1 ]
alt_0:
  %_3 = inttoptr i64 %x8 to ptr
  %_4 = getelementptr inbounds %Node, ptr %_3, i32 1
  %x3271 = load i64, ptr %_4
  %_5 = inttoptr i64 %x8 to ptr
  %_6 = getelementptr inbounds %Node, ptr %_5, i32 2
  %x3273 = load i64, ptr %_6
  %x29 = add i64 %x3271, %x3273
  %_8 = alloca i64
  store i64 1, ptr %_8
  %x3679 = ptrtoint ptr %_8 to i64
  %_7 = insertvalue %Node undef, i64 %x3679, 0
  %alt_0_res = insertvalue %Node %_7, i64 %x29, 1
  br label %continue
alt_1:
  %_9 = inttoptr i64 %x8 to ptr
  %_10 = getelementptr inbounds %Node, ptr %_9, i32 1
  %x3272 = load i64, ptr %_10
  %_12 = alloca i64
  store i64 1, ptr %_12
  %x3680 = ptrtoint ptr %_12 to i64
  %_11 = insertvalue %Node undef, i64 %x3680, 0
  %alt_1_res = insertvalue %Node %_11, i64 %x3272, 1
  br label %continue
alt_default:
  unreachable
continue:
  %_13 = phi %Node [%alt_0_res, %alt_0], [%alt_1_res, %alt_1], [%alt_default_res, %alt_default]
  %x1651 = extractvalue %Node %_13, 1
  %_36 = alloca i64
  store i64 1, ptr %_36
  %x3682 = ptrtoint ptr %_36 to i64
  %_14 = insertvalue %Node undef, i64 %x3682, 0
  %_15 = insertvalue %Node %_14, i64 %x1651, 1
  %_35 = alloca ptr
  store i64 %_15, ptr %_35
  %x3681 = ptrtoint ptr %_35 to i64
  store i64 %x3681, ptr %x8
  switch i64 %x1651, label %alt_default [ i64 0, label %alt_0 ]
alt_0:
  %_16 = insertvalue %Node undef, i64 2, 0
  ret %Node %_16
alt_default:
  %_34 = alloca i64
  store i64 1, ptr %_34
  %x3685 = ptrtoint ptr %_34 to i64
  %_33 = alloca i64
  store i64 1, ptr %_33
  %x3684 = ptrtoint ptr %_33 to i64
  %_17 = insertvalue %Node undef, i64 %x3685, 0
  %_18 = insertvalue %Node %_17, i64 %x3684, 1
  %_32 = alloca ptr
  store i64 %_18, ptr %_32
  %x3683 = ptrtoint ptr %_32 to i64
  %x3 = call fastcc ptr @malloc(i64 192)
  store i64 %x3683, ptr %x3
  %_31 = alloca i64
  store i64 0, ptr %_31
  %x3687 = ptrtoint ptr %_31 to i64
  %_19 = insertvalue %Node undef, i64 %x3687, 0
  %_20 = insertvalue %Node %_19, i64 %x8, 1
  %_21 = insertvalue %Node %_20, i64 %x3, 2
  %_30 = alloca ptr
  store i64 %_21, ptr %_30
  %x3686 = ptrtoint ptr %_30 to i64
  %x2 = call fastcc ptr @malloc(i64 192)
  store i64 %x3686, ptr %x2
  %_29 = alloca i64
  store i64 3, ptr %_29
  %x3689 = ptrtoint ptr %_29 to i64
  %_22 = insertvalue %Node undef, i64 %x3689, 0
  %_23 = insertvalue %Node %_22, i64 %x2, 1
  %_28 = alloca ptr
  store i64 %_23, ptr %_28
  %x3688 = ptrtoint ptr %_28 to i64
  %x5 = call fastcc ptr @malloc(i64 192)
  store i64 %x3688, ptr %x5
  %_27 = alloca i64
  store i64 4, ptr %_27
  %x3690 = ptrtoint ptr %_27 to i64
  %_24 = insertvalue %Node undef, i64 %x3690, 0
  %_25 = insertvalue %Node %_24, i64 %x2, 1
  %_26 = insertvalue %Node %_25, i64 %x5, 2
  ret %Node %_26
}

define fastcc %Node
@"DownFrom.sum"(i64 %x19){
  %_1 = inttoptr i64 %x19 to ptr
  %_2 = getelementptr inbounds %Node, ptr %_1, i32 1
  %x34 = load i64, ptr %_2
  %_3 = call fastcc %Node @"DownFrom.downFrom"(i64 %x34)
  %x1660 = extractvalue %Node %_3, 0
  %x1661 = extractvalue %Node %_3, 1
  %x1662 = extractvalue %Node %_3, 2
  switch i64 %x1660, label %alt_default [ i64 2, label %alt_2
                                          i64 4, label %alt_4 ]
alt_2:
  %_4 = insertvalue %Node undef, i64 %x1660, 0
  %_5 = insertvalue %Node %_4, i64 %x1661, 1
  %_6 = insertvalue %Node %_5, i64 %x1662, 2
  %_11 = alloca ptr
  store i64 %_6, ptr %_11
  %x3691 = ptrtoint ptr %_11 to i64
  store i64 %x3691, ptr %x19
  %_10 = alloca i64
  store i64 1, ptr %_10
  %x3693 = ptrtoint ptr %_10 to i64
  %_9 = alloca i64
  store i64 0, ptr %_9
  %x3692 = ptrtoint ptr %_9 to i64
  %_7 = insertvalue %Node undef, i64 %x3693, 0
  %_8 = insertvalue %Node %_7, i64 %x3692, 1
  ret %Node %_8
alt_4:
  %_12 = insertvalue %Node undef, i64 %x1660, 0
  %_13 = insertvalue %Node %_12, i64 %x1661, 1
  %_14 = insertvalue %Node %_13, i64 %x1662, 2
  %_46 = alloca ptr
  store i64 %_14, ptr %_46
  %x3694 = ptrtoint ptr %_46 to i64
  store i64 %x3694, ptr %x19
  %_45 = alloca i64
  store i64 5, ptr %_45
  %x3696 = ptrtoint ptr %_45 to i64
  %_15 = insertvalue %Node undef, i64 %x3696, 0
  %_16 = insertvalue %Node %_15, i64 %x1662, 1
  %_44 = alloca ptr
  store i64 %_16, ptr %_44
  %x3695 = ptrtoint ptr %_44 to i64
  %x11 = call fastcc ptr @malloc(i64 192)
  store i64 %x3695, ptr %x11
  %_17 = inttoptr i64 %x11 to ptr
  %_18 = getelementptr inbounds %Node, ptr %_17, i32 1
  %x36 = load i64, ptr %_18
  %_19 = call fastcc %Node @"DownFrom.sum"(i64 %x36)
  %x1659 = extractvalue %Node %_19, 1
  %_43 = alloca i64
  store i64 1, ptr %_43
  %x3698 = ptrtoint ptr %_43 to i64
  %_20 = insertvalue %Node undef, i64 %x3698, 0
  %_21 = insertvalue %Node %_20, i64 %x1659, 1
  %_42 = alloca ptr
  store i64 %_21, ptr %_42
  %x3697 = ptrtoint ptr %_42 to i64
  store i64 %x3697, ptr %x11
  %_22 = inttoptr i64 %x1661 to ptr
  %_23 = getelementptr inbounds %Node, ptr %_22, i32 0
  %x1656 = load i64, ptr %_23
  switch i64 %x1656, label %alt_default [ i64 0, label %alt_0
                                          i64 1, label %alt_1 ]
alt_0:
  %_24 = inttoptr i64 %x1661 to ptr
  %_25 = getelementptr inbounds %Node, ptr %_24, i32 1
  %x3274 = load i64, ptr %_25
  %_26 = inttoptr i64 %x1661 to ptr
  %_27 = getelementptr inbounds %Node, ptr %_26, i32 2
  %x3276 = load i64, ptr %_27
  %x39 = add i64 %x3274, %x3276
  %_29 = alloca i64
  store i64 1, ptr %_29
  %x3699 = ptrtoint ptr %_29 to i64
  %_28 = insertvalue %Node undef, i64 %x3699, 0
  %alt_0_res = insertvalue %Node %_28, i64 %x39, 1
  br label %continue
alt_1:
  %_30 = inttoptr i64 %x1661 to ptr
  %_31 = getelementptr inbounds %Node, ptr %_30, i32 1
  %x3275 = load i64, ptr %_31
  %_33 = alloca i64
  store i64 1, ptr %_33
  %x3700 = ptrtoint ptr %_33 to i64
  %_32 = insertvalue %Node undef, i64 %x3700, 0
  %alt_1_res = insertvalue %Node %_32, i64 %x3275, 1
  br label %continue
alt_default:
  unreachable
continue:
  %_34 = phi %Node [%alt_0_res, %alt_0], [%alt_1_res, %alt_1], [%alt_default_res, %alt_default]
  %x1655 = extractvalue %Node %_34, 1
  %_41 = alloca i64
  store i64 1, ptr %_41
  %x3702 = ptrtoint ptr %_41 to i64
  %_35 = insertvalue %Node undef, i64 %x3702, 0
  %_36 = insertvalue %Node %_35, i64 %x1655, 1
  %_40 = alloca ptr
  store i64 %_36, ptr %_40
  %x3701 = ptrtoint ptr %_40 to i64
  store i64 %x3701, ptr %x1661
  %x13 = add i64 %x1659, %x1655
  %_39 = alloca i64
  store i64 1, ptr %_39
  %x3703 = ptrtoint ptr %_39 to i64
  %_37 = insertvalue %Node undef, i64 %x3703, 0
  %_38 = insertvalue %Node %_37, i64 %x13, 1
  ret %Node %_38
alt_default:
  unreachable
}

define fastcc void
@main(){
  %_10 = alloca i64
  store i64 1, ptr %_10
  %x3706 = ptrtoint ptr %_10 to i64
  %_9 = alloca i64
  store i64 100, ptr %_9
  %x3705 = ptrtoint ptr %_9 to i64
  %_1 = insertvalue %Node undef, i64 %x3706, 0
  %_2 = insertvalue %Node %_1, i64 %x3705, 1
  %_8 = alloca ptr
  store i64 %_2, ptr %_8
  %x3704 = ptrtoint ptr %_8 to i64
  %x24 = call fastcc ptr @malloc(i64 192)
  store i64 %x3704, ptr %x24
  %_7 = alloca i64
  store i64 3, ptr %_7
  %x3708 = ptrtoint ptr %_7 to i64
  %_3 = insertvalue %Node undef, i64 %x3708, 0
  %_4 = insertvalue %Node %_3, i64 %x24, 1
  %_6 = alloca ptr
  store i64 %_4, ptr %_6
  %x3707 = ptrtoint ptr %_6 to i64
  %x23 = call fastcc ptr @malloc(i64 192)
  store i64 %x3707, ptr %x23
  %_5 = call fastcc %Node @"DownFrom.sum"(i64 %x23)
  %x25 = extractvalue %Node %_5, 1
  call fastcc void @printf(ptr @"%d", i64 %x25)
  ret void
}