	.text
	.file	"ld-temp.o"
	.section	".text.Agda.Builtin.Nat._-_","ax",@progbits
	.p2align	4, 0x90                         # -- Begin function Agda.Builtin.Nat._-_
	.type	"Agda.Builtin.Nat._-_",@function
"Agda.Builtin.Nat._-_":                 # @Agda.Builtin.Nat._-_
# %bb.0:
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$40, %rsp
	movq	%rdx, %r14
	movq	%rsi, %r12
	movq	%rdi, %rbx
	cmpq	$0, 8(%rsi)
	movq	16(%rsi), %r15
	je	.LBB0_2
# %bb.1:
	movq	24(%r12), %rdx
	leaq	8(%rsp), %rdi
	movq	%r15, %rsi
	callq	"Agda.Builtin.Nat._-_"
	movq	24(%rsp), %r15
.LBB0_2:
	movq	$0, 8(%r12)
	movq	%r15, 16(%r12)
	movq	(%r12), %rax
	cmpq	$1, %rax
	jne	.LBB0_4
# %bb.3:
	movq	%r12, %rdi
	callq	free@PLT
	jmp	.LBB0_5
.LBB0_4:
	decq	%rax
	movq	%rax, (%r12)
.LBB0_5:
	movq	(%r14), %rax
	movq	16(%r14), %r12
	movq	$0, 8(%r14)
	cmpq	$1, %rax
	jne	.LBB0_7
# %bb.6:
	movq	%r14, %rdi
	callq	free@PLT
	jmp	.LBB0_8
.LBB0_7:
	decq	%rax
	movq	%rax, (%r14)
.LBB0_8:
	subq	%r12, %r15
	movq	%r15, 16(%rbx)
	movq	$0, 8(%rbx)
	movq	%rbx, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Lfunc_end0:
	.size	"Agda.Builtin.Nat._-_", .Lfunc_end0-"Agda.Builtin.Nat._-_"
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4, 0x0                          # -- Begin function DownFrom.sum
.LCPI1_0:
	.quad	1                               # 0x1
	.quad	0                               # 0x0
.LCPI1_1:
	.quad	2                               # 0x2
	.quad	1                               # 0x1
.LCPI1_2:
	.quad	1                               # 0x1
	.quad	3                               # 0x3
	.section	.text.DownFrom.sum,"ax",@progbits
	.p2align	4, 0x90
	.type	DownFrom.sum,@function
DownFrom.sum:                           # @DownFrom.sum
# %bb.0:
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$72, %rsp
	movq	%rdi, %rbx
	movq	16(%rdi), %r15
	cmpq	$0, 8(%r15)
	movq	16(%r15), %rsi
	je	.LBB1_2
# %bb.1:
	movq	24(%r15), %rdx
	leaq	40(%rsp), %rdi
	callq	"Agda.Builtin.Nat._-_"
	movq	56(%rsp), %rsi
.LBB1_2:
	movq	$0, 8(%r15)
	movq	%rsi, 16(%r15)
	testq	%rsi, %rsi
	je	.LBB1_4
# %bb.3:
	movl	$32, %edi
	callq	malloc@PLT
	movq	%rax, %r12
	movaps	.LCPI1_0(%rip), %xmm0           # xmm0 = [1,0]
	movups	%xmm0, (%rax)
	movq	$1, 16(%rax)
	movl	$32, %edi
	callq	malloc@PLT
	movq	%rax, %r14
	movq	%r15, 16(%rax)
	movq	%r12, 24(%rax)
	movaps	.LCPI1_1(%rip), %xmm0           # xmm0 = [2,1]
	movups	%xmm0, (%rax)
	movl	$32, %edi
	callq	malloc@PLT
	movq	%rax, %r15
	movaps	.LCPI1_2(%rip), %xmm0           # xmm0 = [1,3]
	movups	%xmm0, (%rax)
	movq	%r14, 16(%rax)
	movl	$4, %eax
	cmpq	$2, %rax
	jne	.LBB1_7
.LBB1_16:
	movq	$2, 8(%rbx)
	movq	(%rbx), %rax
	cmpq	$1, %rax
	jne	.LBB1_18
# %bb.17:
	movq	%rbx, %rdi
	callq	free@PLT
	jmp	.LBB1_19
.LBB1_4:
	movq	(%r15), %rax
	cmpq	$1, %rax
	jne	.LBB1_15
# %bb.5:
	movq	%r15, %rdi
	callq	free@PLT
	movl	$2, %eax
                                        # implicit-def: $r14
                                        # implicit-def: $r15
	cmpq	$2, %rax
	je	.LBB1_16
	jmp	.LBB1_7
.LBB1_18:
	decq	%rax
	movq	%rax, (%rbx)
.LBB1_19:
	xorl	%ebx, %ebx
	jmp	.LBB1_20
.LBB1_15:
	decq	%rax
	movq	%rax, (%r15)
	movl	$2, %eax
                                        # implicit-def: $r14
                                        # implicit-def: $r15
	cmpq	$2, %rax
	je	.LBB1_16
.LBB1_7:
	movq	$4, 8(%rbx)
	movq	%r14, 16(%rbx)
	movq	%r15, 24(%rbx)
	movq	(%rbx), %rax
	cmpq	$1, %rax
	jne	.LBB1_9
# %bb.8:
	movq	%rbx, %rdi
	callq	free@PLT
	jmp	.LBB1_10
.LBB1_9:
	incq	(%r15)
	incq	(%r14)
	decq	%rax
	movq	%rax, (%rbx)
.LBB1_10:
	movq	%r15, %rdi
	callq	DownFrom.sum
	movq	%rax, %r15
	cmpq	$0, 8(%r14)
	movq	16(%r14), %rbx
	je	.LBB1_12
# %bb.11:
	movq	24(%r14), %rdx
	leaq	8(%rsp), %rdi
	movq	%rbx, %rsi
	callq	"Agda.Builtin.Nat._-_"
	movq	24(%rsp), %rbx
.LBB1_12:
	movq	$0, 8(%r14)
	movq	%rbx, 16(%r14)
	movq	(%r14), %rax
	cmpq	$1, %rax
	jne	.LBB1_14
# %bb.13:
	movq	%r14, %rdi
	callq	free@PLT
	addq	%r15, %rbx
	jmp	.LBB1_20
.LBB1_14:
	decq	%rax
	movq	%rax, (%r14)
	addq	%r15, %rbx
.LBB1_20:
	movq	%rbx, %rax
	addq	$72, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Lfunc_end1:
	.size	DownFrom.sum, .Lfunc_end1-DownFrom.sum
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4, 0x0                          # -- Begin function main
.LCPI2_0:
	.long	1                               # 0x1
	.long	0                               # 0x0
	.long	0                               # 0x0
	.long	0                               # 0x0
.LCPI2_1:
	.quad	1                               # 0x1
	.quad	3                               # 0x3
	.section	.text.main,"ax",@progbits
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
.Lmain$local:
	.type	.Lmain$local,@function
	.cfi_startproc
# %bb.0:
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movl	$32, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movaps	.LCPI2_0(%rip), %xmm0           # xmm0 = [1,0,0,0]
	movups	%xmm0, (%rax)
	movq	$100, 16(%rax)
	movl	$32, %edi
	callq	malloc@PLT
	movaps	.LCPI2_1(%rip), %xmm0           # xmm0 = [1,3]
	movups	%xmm0, (%rax)
	movq	%rbx, 16(%rax)
	movq	%rax, %rdi
	callq	DownFrom.sum
	leaq	".L%d"(%rip), %rdi
	movq	%rax, %rsi
	popq	%rbx
	.cfi_def_cfa_offset 8
	jmp	printf@PLT                      # TAILCALL
.Lfunc_end2:
	.size	main, .Lfunc_end2-main
	.size	.Lmain$local, .Lfunc_end2-main
	.cfi_endproc
                                        # -- End function
	.section	.text.dup,"ax",@progbits
	.globl	dup                             # -- Begin function dup
	.p2align	4, 0x90
	.type	dup,@function
dup:                                    # @dup
.Ldup$local:
	.type	.Ldup$local,@function
# %bb.0:
	incq	(%rdi)
	retq
.Lfunc_end3:
	.size	dup, .Lfunc_end3-dup
	.size	.Ldup$local, .Lfunc_end3-dup
                                        # -- End function
	.type	".L%d",@object                  # @"%d"
	.section	".rodata..L%d","a",@progbits
".L%d":
	.asciz	"%d\n"
	.size	".L%d", 4

	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym ".L%d"
