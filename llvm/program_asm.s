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
	.quad	1                               # 0x1
.LCPI0_2:
	.quad	1                               # 0x1
	.quad	3                               # 0x3
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
# %bb.8:                                # %default_1
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
	movaps	xmm0, xmmword ptr [rip + .LCPI0_1] # xmm0 = [2,1]
	movups	xmmword ptr [rax], xmm0
	mov	edi, 32
	call	malloc@PLT
	movaps	xmm0, xmmword ptr [rip + .LCPI0_2] # xmm0 = [1,3]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], r15
	mov	ecx, 4
	jmp	.LBB0_7
.LBB0_3:                                # %"0_1"
	mov	rax, qword ptr [rbx]
	cmp	rax, 1
	jne	.LBB0_5
# %bb.4:                                # %"1_3"
	mov	rdi, rbx
	call	free@PLT
	jmp	.LBB0_6
.LBB0_5:                                # %default_3
	dec	rax
	mov	qword ptr [rbx], rax
.LBB0_6:                                # %common.ret
	mov	ecx, 2
                                        # implicit-def: $r15
                                        # implicit-def: $rax
.LBB0_7:                                # %common.ret
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
	.globl	DownFrom.sum                    # -- Begin function DownFrom.sum
	.p2align	4, 0x90
	.type	DownFrom.sum,@function
DownFrom.sum:                           # @DownFrom.sum
# %bb.0:
	push	r15
	push	r14
	push	r12
	push	rbx
	sub	rsp, 104
	mov	rbx, rsi
	mov	r14, rdi
	mov	rsi, qword ptr [rsi + 16]
	lea	rdi, [rsp + 8]
	call	DownFrom.downFrom@PLT
	cmp	qword ptr [rsp + 16], 2
	jne	.LBB1_6
# %bb.1:                                # %"CDownFrom.List.[]_4"
	mov	qword ptr [rbx + 8], 2
	mov	rax, qword ptr [rbx]
	cmp	rax, 1
	jne	.LBB1_3
# %bb.2:                                # %"1_6"
	mov	rdi, rbx
	call	free@PLT
	jmp	.LBB1_4
.LBB1_6:                                # %"CDownFrom.List._\E2\88\B7__4"
	mov	r12, qword ptr [rsp + 32]
	mov	r15, qword ptr [rsp + 24]
	mov	qword ptr [rbx + 8], 4
	mov	qword ptr [rbx + 16], r15
	mov	qword ptr [rbx + 24], r12
	mov	rax, qword ptr [rbx]
	cmp	rax, 1
	jne	.LBB1_8
# %bb.7:                                # %"1_8"
	mov	rdi, rbx
	call	free@PLT
	jmp	.LBB1_9
.LBB1_3:                                # %default_6
	dec	rax
	mov	qword ptr [rbx], rax
.LBB1_4:                                # %common.ret
	xor	eax, eax
	xor	ebx, ebx
	jmp	.LBB1_5
.LBB1_8:                                # %default_8
	inc	qword ptr [r12]
	inc	qword ptr [r15]
	dec	rax
	mov	qword ptr [rbx], rax
.LBB1_9:                                # %continue_9.i
	lea	rdi, [rsp + 72]
	mov	rsi, r12
	call	DownFrom.sum@PLT
	mov	r12, qword ptr [rsp + 88]
	cmp	qword ptr [r15 + 8], 0
	mov	rbx, qword ptr [r15 + 16]
	je	.LBB1_11
# %bb.10:                               # %FAgda.Builtin.Nat._-__11.i
	mov	rdx, qword ptr [r15 + 24]
	lea	rdi, [rsp + 40]
	mov	rsi, rbx
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rbx, qword ptr [rsp + 56]
.LBB1_11:                               # %continue_11.i
	mov	qword ptr [r15 + 8], 0
	mov	qword ptr [r15 + 16], rbx
	mov	rax, qword ptr [r15]
	cmp	rax, 1
	jne	.LBB1_13
# %bb.12:                               # %"1_13.i"
	mov	rdi, r15
	call	free@PLT
	jmp	.LBB1_14
.LBB1_13:                               # %default_13.i
	dec	rax
	mov	qword ptr [r15], rax
.LBB1_14:                               # %"Agda.Builtin.Nat._+_.exit"
	add	rbx, r12
	xor	eax, eax
.LBB1_5:                                # %common.ret
	mov	qword ptr [r14], rax
	mov	qword ptr [r14 + 8], rax
	mov	qword ptr [r14 + 16], rbx
	mov	qword ptr [r14 + 24], rax
	mov	rax, r14
	add	rsp, 104
	pop	rbx
	pop	r12
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
	push	r12
	push	rbx
	sub	rsp, 72
	mov	r15, rdx
	mov	rbx, rsi
	mov	r14, rdi
	mov	rsi, qword ptr [rsi + 16]
	lea	rdi, [rsp + 40]
	call	DownFrom.sum@PLT
	mov	r12, qword ptr [rsp + 56]
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], r12
	mov	rax, qword ptr [rbx]
	cmp	rax, 1
	jne	.LBB3_2
# %bb.1:                                # %"1_10"
	mov	rdi, rbx
	call	free@PLT
	cmp	qword ptr [r15 + 8], 0
	mov	rbx, qword ptr [r15 + 16]
	jne	.LBB3_4
	jmp	.LBB3_5
.LBB3_2:                                # %default_10
	dec	rax
	mov	qword ptr [rbx], rax
	cmp	qword ptr [r15 + 8], 0
	mov	rbx, qword ptr [r15 + 16]
	je	.LBB3_5
.LBB3_4:                                # %FAgda.Builtin.Nat._-__11
	mov	rdx, qword ptr [r15 + 24]
	lea	rdi, [rsp + 8]
	mov	rsi, rbx
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rbx, qword ptr [rsp + 24]
.LBB3_5:                                # %continue_11
	mov	qword ptr [r15 + 8], 0
	mov	qword ptr [r15 + 16], rbx
	mov	rax, qword ptr [r15]
	cmp	rax, 1
	jne	.LBB3_7
# %bb.6:                                # %"1_13"
	mov	rdi, r15
	call	free@PLT
	jmp	.LBB3_8
.LBB3_7:                                # %default_13
	dec	rax
	mov	qword ptr [r15], rax
.LBB3_8:                                # %continue_12
	add	rbx, r12
	mov	qword ptr [r14 + 16], rbx
	mov	qword ptr [r14 + 8], 0
	mov	rax, r14
	add	rsp, 72
	pop	rbx
	pop	r12
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
# %bb.1:                                # %FAgda.Builtin.Nat._-__14
	mov	rdx, qword ptr [rbx + 24]
	lea	rdi, [rsp + 8]
	mov	rsi, r12
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	r12, qword ptr [rsp + 24]
.LBB4_2:                                # %continue_14
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], r12
	mov	rax, qword ptr [rbx]
	cmp	rax, 1
	jne	.LBB4_4
# %bb.3:                                # %"1_16"
	mov	rdi, rbx
	call	free@PLT
	jmp	.LBB4_5
.LBB4_4:                                # %default_16
	dec	rax
	mov	qword ptr [rbx], rax
.LBB4_5:                                # %continue_15
	mov	rax, qword ptr [r15]
	mov	rbx, qword ptr [r15 + 16]
	mov	qword ptr [r15 + 8], 0
	cmp	rax, 1
	jne	.LBB4_7
# %bb.6:                                # %"1_18"
	mov	rdi, r15
	call	free@PLT
	jmp	.LBB4_8
.LBB4_7:                                # %default_18
	dec	rax
	mov	qword ptr [r15], rax
.LBB4_8:                                # %continue_17
	sub	r12, rbx
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
# %bb.1:                                # %"1_19"
	mov	rax, qword ptr [rbx + 8]
	lea	rcx, [rip + .LJTI5_0]
	movsxd	rax, dword ptr [rcx + 4*rax]
	add	rax, rcx
	jmp	rax
.LBB5_2:                                # %"CDownFrom.List._\E2\88\B7__20"
	mov	rdi, qword ptr [rbx + 16]
	call	drop@PLT
	mov	rdi, qword ptr [rbx + 24]
	jmp	.LBB5_4
.LBB5_6:                                # %default_19
	dec	rax
	mov	qword ptr [rbx], rax
	pop	rbx
	ret
.LBB5_3:                                # %FDownFrom.sum_20
	mov	rdi, qword ptr [rbx + 16]
.LBB5_4:                                # %FDownFrom.sum_20
	call	drop@PLT
.LBB5_5:                                # %"CDownFrom.List.[]_20"
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
