	.text
	.intel_syntax noprefix
	.file	"program.ll"
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4                               # -- Begin function DownFrom.downFrom
.LCPI0_0:
	.quad	1                               # 0x1
	.quad	0                               # 0x0
.LCPI0_1:
	.quad	2                               # 0x2
	.quad	3                               # 0x3
.LCPI0_2:
	.quad	3                               # 0x3
	.quad	1                               # 0x1
	.text
	.globl	DownFrom.downFrom
	.p2align	4, 0x90
	.type	DownFrom.downFrom,@function
DownFrom.downFrom:                      # @DownFrom.downFrom
# %bb.0:
	push	r15
	push	r14
	push	r12
	push	rbx
	sub	rsp, 40
	mov	rbx, rsi
	mov	r14, rdi
	cmp	qword ptr [rsi + 8], 0
	mov	rsi, qword ptr [rsi + 16]
	je	.LBB0_2
# %bb.1:                                # %FAgda.Builtin.Nat._-__0
	mov	rdx, qword ptr [rbx + 24]
	lea	rdi, [rsp + 8]
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rsi, qword ptr [rsp + 24]
.LBB0_2:                                # %continue_0
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], rsi
	test	rsi, rsi
	je	.LBB0_3
# %bb.4:                                # %default_1
	mov	edi, 32
	call	malloc@PLT
	mov	r12, rax
	movaps	xmm0, xmmword ptr [rip + .LCPI0_0] # xmm0 = [1,0]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], 1
	mov	edi, 32
	call	malloc@PLT
	mov	r15, rax
	mov	qword ptr [rax + 16], rbx
	mov	qword ptr [rax + 24], r12
	mov	edi, 32
	call	malloc@PLT
	mov	qword ptr [rax + 16], r15
	movaps	xmm0, xmmword ptr [rip + .LCPI0_1] # xmm0 = [2,3]
	movups	xmmword ptr [rax], xmm0
	movaps	xmm0, xmmword ptr [rip + .LCPI0_2] # xmm0 = [3,1]
	movups	xmmword ptr [r15], xmm0
	mov	ecx, 4
	jmp	.LBB0_5
.LBB0_3:                                # %"0_1"
	mov	rdi, rbx
	call	drop@PLT
	mov	ecx, 2
                                        # implicit-def: $r15
                                        # implicit-def: $rax
.LBB0_5:                                # %common.ret
	mov	qword ptr [r14], rax
	mov	qword ptr [r14 + 8], rcx
	mov	qword ptr [r14 + 16], r15
	mov	qword ptr [r14 + 24], rax
	mov	rax, r14
	add	rsp, 40
	pop	rbx
	pop	r12
	pop	r14
	pop	r15
	ret
.Lfunc_end0:
	.size	DownFrom.downFrom, .Lfunc_end0-DownFrom.downFrom
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4                               # -- Begin function DownFrom.sum
.LCPI1_0:
	.quad	1                               # 0x1
	.quad	0                               # 0x0
.LCPI1_1:
	.quad	2                               # 0x2
	.quad	3                               # 0x3
.LCPI1_2:
	.quad	3                               # 0x3
	.quad	1                               # 0x1
	.text
	.globl	DownFrom.sum
	.p2align	4, 0x90
	.type	DownFrom.sum,@function
DownFrom.sum:                           # @DownFrom.sum
# %bb.0:
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	sub	rsp, 96
	mov	r15, rsi
	mov	r14, rdi
	mov	rbx, qword ptr [rsi + 16]
	cmp	qword ptr [rbx + 8], 0
	mov	rsi, qword ptr [rbx + 16]
	je	.LBB1_2
# %bb.1:                                # %FAgda.Builtin.Nat._-__0.i
	mov	rdx, qword ptr [rbx + 24]
	lea	rdi, [rsp + 64]
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rsi, qword ptr [rsp + 80]
.LBB1_2:                                # %continue_0.i
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], rsi
	test	rsi, rsi
	je	.LBB1_3
# %bb.4:                                # %default_1.i
	mov	edi, 32
	call	malloc@PLT
	mov	r12, rax
	movaps	xmm0, xmmword ptr [rip + .LCPI1_0] # xmm0 = [1,0]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], 1
	mov	edi, 32
	call	malloc@PLT
	mov	r13, rax
	mov	qword ptr [rax + 16], rbx
	mov	qword ptr [rax + 24], r12
	mov	edi, 32
	call	malloc@PLT
	mov	r12, rax
	mov	qword ptr [rax + 16], r13
	movaps	xmm0, xmmword ptr [rip + .LCPI1_1] # xmm0 = [2,3]
	movups	xmmword ptr [rax], xmm0
	movaps	xmm0, xmmword ptr [rip + .LCPI1_2] # xmm0 = [3,1]
	movups	xmmword ptr [r13], xmm0
	mov	eax, 4
	cmp	rax, 2
	jne	.LBB1_7
.LBB1_6:                                # %"CDownFrom.List.[]_2"
	mov	qword ptr [r15 + 8], 2
	mov	qword ptr [r15 + 16], r13
	mov	qword ptr [r15 + 24], r12
	mov	rdi, r15
	call	drop@PLT
	xor	eax, eax
	xor	r15d, r15d
	jmp	.LBB1_10
.LBB1_3:                                # %"0_1.i"
	mov	rdi, rbx
	call	drop@PLT
	mov	eax, 2
                                        # implicit-def: $r13
                                        # implicit-def: $r12
	cmp	rax, 2
	je	.LBB1_6
.LBB1_7:                                # %"CDownFrom.List._\E2\88\B7__2"
	mov	qword ptr [r15 + 8], rax
	mov	qword ptr [r15 + 16], r13
	mov	qword ptr [r15 + 24], r12
	mov	rdi, r15
	call	drop@PLT
	mov	edi, 32
	call	malloc@PLT
	mov	rbx, rax
	mov	qword ptr [rax], 1
	mov	qword ptr [rax + 8], 5
	mov	qword ptr [rax + 16], r12
	lea	rdi, [rsp + 32]
	mov	rsi, r12
	call	DownFrom.sum@PLT
	mov	r15, qword ptr [rsp + 48]
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], r15
	mov	rdi, rbx
	call	drop@PLT
	cmp	qword ptr [r13 + 8], 0
	mov	rbx, qword ptr [r13 + 16]
	je	.LBB1_9
# %bb.8:                                # %FAgda.Builtin.Nat._-__3.i
	mov	rdx, qword ptr [r13 + 24]
	mov	rdi, rsp
	mov	rsi, rbx
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rbx, qword ptr [rsp + 16]
.LBB1_9:                                # %"Agda.Builtin.Nat._+_.exit"
	mov	qword ptr [r13 + 8], 0
	mov	qword ptr [r13 + 16], rbx
	mov	rdi, r13
	call	drop@PLT
	add	r15, rbx
	xor	eax, eax
.LBB1_10:                               # %common.ret
	mov	qword ptr [r14], rax
	mov	qword ptr [r14 + 8], rax
	mov	qword ptr [r14 + 16], r15
	mov	qword ptr [r14 + 24], rax
	mov	rax, r14
	add	rsp, 96
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	ret
.Lfunc_end1:
	.size	DownFrom.sum, .Lfunc_end1-DownFrom.sum
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4                               # -- Begin function main
.LCPI2_0:
	.long	1                               # 0x1
	.long	0                               # 0x0
	.long	0                               # 0x0
	.long	0                               # 0x0
.LCPI2_1:
	.quad	1                               # 0x1
	.quad	3                               # 0x3
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	push	rbx
	.cfi_def_cfa_offset 16
	sub	rsp, 32
	.cfi_def_cfa_offset 48
	.cfi_offset rbx, -16
	mov	edi, 32
	call	malloc@PLT
	mov	rbx, rax
	movaps	xmm0, xmmword ptr [rip + .LCPI2_0] # xmm0 = [1,0,0,0]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], 100
	mov	edi, 32
	call	malloc@PLT
	movaps	xmm0, xmmword ptr [rip + .LCPI2_1] # xmm0 = [1,3]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], rbx
	mov	rdi, rsp
	mov	rsi, rax
	call	DownFrom.sum@PLT
	mov	rsi, qword ptr [rsp + 16]
	lea	rdi, [rip + ".L%d"]
	add	rsp, 32
	.cfi_def_cfa_offset 16
	pop	rbx
	.cfi_def_cfa_offset 8
	jmp	printf@PLT                      # TAILCALL
.Lfunc_end2:
	.size	main, .Lfunc_end2-main
	.cfi_endproc
                                        # -- End function
	.globl	"Agda.Builtin.Nat._+_"          # -- Begin function Agda.Builtin.Nat._+_
	.p2align	4, 0x90
	.type	"Agda.Builtin.Nat._+_",@function
"Agda.Builtin.Nat._+_":                 # @"Agda.Builtin.Nat._+_"
# %bb.0:
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	sub	rsp, 64
	mov	r12, rdx
	mov	rbx, rsi
	mov	r14, rdi
	mov	rsi, qword ptr [rsi + 16]
	lea	rdi, [rsp + 32]
	call	DownFrom.sum@PLT
	mov	r13, qword ptr [rsp + 48]
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], r13
	mov	rdi, rbx
	call	drop@PLT
	cmp	qword ptr [r12 + 8], 0
	mov	r15, qword ptr [r12 + 16]
	je	.LBB3_2
# %bb.1:                                # %FAgda.Builtin.Nat._-__3
	mov	rdx, qword ptr [r12 + 24]
	mov	rdi, rsp
	mov	rsi, r15
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	r15, qword ptr [rsp + 16]
.LBB3_2:                                # %continue_3
	mov	qword ptr [r12 + 8], 0
	mov	qword ptr [r12 + 16], r15
	mov	rdi, r12
	call	drop@PLT
	add	r13, r15
	mov	qword ptr [r14 + 16], r13
	mov	qword ptr [r14 + 8], 0
	mov	rax, r14
	add	rsp, 64
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	ret
.Lfunc_end3:
	.size	"Agda.Builtin.Nat._+_", .Lfunc_end3-"Agda.Builtin.Nat._+_"
                                        # -- End function
	.globl	"Agda.Builtin.Nat._-_"          # -- Begin function Agda.Builtin.Nat._-_
	.p2align	4, 0x90
	.type	"Agda.Builtin.Nat._-_",@function
"Agda.Builtin.Nat._-_":                 # @Agda.Builtin.Nat._-_
# %bb.0:
	push	r15
	push	r14
	push	r12
	push	rbx
	sub	rsp, 40
	mov	r15, rdx
	mov	rbx, rsi
	mov	r14, rdi
	cmp	qword ptr [rsi + 8], 0
	mov	r12, qword ptr [rsi + 16]
	je	.LBB4_2
# %bb.1:                                # %FAgda.Builtin.Nat._-__4
	mov	rdx, qword ptr [rbx + 24]
	lea	rdi, [rsp + 8]
	mov	rsi, r12
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	r12, qword ptr [rsp + 24]
.LBB4_2:                                # %continue_4
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], r12
	mov	rdi, rbx
	call	drop@PLT
	mov	qword ptr [r15 + 8], 0
	sub	r12, qword ptr [r15 + 16]
	mov	rdi, r15
	call	drop@PLT
	mov	qword ptr [r14 + 16], r12
	mov	qword ptr [r14 + 8], 0
	mov	rax, r14
	add	rsp, 40
	pop	rbx
	pop	r12
	pop	r14
	pop	r15
	ret
.Lfunc_end4:
	.size	"Agda.Builtin.Nat._-_", .Lfunc_end4-"Agda.Builtin.Nat._-_"
                                        # -- End function
	.globl	drop                            # -- Begin function drop
	.p2align	4, 0x90
	.type	drop,@function
drop:                                   # @drop
# %bb.0:
	push	rbx
	mov	rbx, rdi
	mov	rax, qword ptr [rdi]
	cmp	rax, 1
	jne	.LBB5_6
# %bb.1:                                # %"1_5"
	mov	rax, qword ptr [rbx + 8]
	lea	rcx, [rip + .LJTI5_0]
	movsxd	rax, dword ptr [rcx + 4*rax]
	add	rax, rcx
	jmp	rax
.LBB5_2:                                # %"CDownFrom.List._\E2\88\B7__6"
	mov	rdi, qword ptr [rbx + 16]
	call	drop@PLT
	mov	rdi, qword ptr [rbx + 24]
	jmp	.LBB5_4
.LBB5_6:                                # %default_5
	dec	rax
	mov	qword ptr [rbx], rax
	pop	rbx
	ret
.LBB5_3:                                # %FDownFrom.sum_6
	mov	rdi, qword ptr [rbx + 16]
.LBB5_4:                                # %FDownFrom.sum_6
	call	drop@PLT
.LBB5_5:                                # %"CDownFrom.List.[]_6"
	mov	rdi, rbx
	pop	rbx
	jmp	free@PLT                        # TAILCALL
.Lfunc_end5:
	.size	drop, .Lfunc_end5-drop
	.section	.rodata,"a",@progbits
	.p2align	2
.LJTI5_0:
	.long	.LBB5_5-.LJTI5_0
	.long	.LBB5_2-.LJTI5_0
	.long	.LBB5_5-.LJTI5_0
	.long	.LBB5_3-.LJTI5_0
	.long	.LBB5_2-.LJTI5_0
	.long	.LBB5_3-.LJTI5_0
                                        # -- End function
	.text
	.globl	dup                             # -- Begin function dup
	.p2align	4, 0x90
	.type	dup,@function
dup:                                    # @dup
# %bb.0:
	inc	qword ptr [rdi]
	ret
.Lfunc_end6:
	.size	dup, .Lfunc_end6-dup
                                        # -- End function
	.type	".L%d",@object                  # @"%d"
	.section	.rodata,"a",@progbits
".L%d":
	.asciz	"%d\n"
	.size	".L%d", 4

	.section	".note.GNU-stack","",@progbits
