target triple = "x86_64-pc-linux-gnu"
target datalayout = "p:64:64:64"
declare void @printf(ptr, ...)
@"%d" = private constant [4 x i8] c"%d\0A\00", align 1


define fastcc void
@main() {
  switch i64 2, label %alt_default [ i64 0, label %alt_0
                                     i64 1, label %alt_1 ]

alt_0:
  %1 = add i64 0, 0
  br label %continue

alt_1:
  %2 = add i64 1, 0
  br label %continue

alt_default:
  %3 = add i64 1, 9
  br label %continue

continue:
  %x = phi i64 [%1, %alt_0], [%2, %alt_1], [%3, %alt_default]
  %y = %x
  call void @printf(ptr @"%d", i64 %x)
  ret void
}
