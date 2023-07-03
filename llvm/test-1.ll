target triple = "x86_64-pc-linux-gnu"

;; pretty printing
@pp_int = private constant [4 x i8] c"%d\0A\00", align 1
@pp_msg = private constant [19 x i8] c"Värde på heap: \0A\00", align 1
@pp_msg2 = private constant [14 x i8] c"Efter free: \0A\00", align 1

;; libc
declare ptr @malloc(i64)
declare void @free(ptr)
declare void @printf(ptr, ...)
declare void @putchar(i64)


define i64 @main() {
    %1 = add i64 3, 5
    %2 = call ptr @malloc(i64 4)
    store i64 123, ptr %2
    %3 = load i64, ptr %2
	call void @printf(ptr @pp_msg)
	call void @printf(ptr @pp_int, i64 noundef %3)
    call void @free(ptr %2)
    %4 = load i64, ptr %2
	call void @printf(ptr @pp_msg2, i64 noundef %3)
	call void @printf(ptr @pp_int, i64 noundef %4)
    ret i64 0
}

