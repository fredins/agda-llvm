	.text
	.intel_syntax noprefix
	.file	"program.ll"
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
	mov	r12, rdx
	mov	rbx, rsi
	mov	r14, rdi
	cmp	qword ptr [rsi + 8], 0
	mov	r15, qword ptr [rsi + 16]
	je	.LBB0_2
# %bb.1:                                # %"FAgda.Builtin.Nat._+__0"
	mov	rdx, qword ptr [rbx + 24]
	lea	rdi, [rsp + 40]
	mov	rsi, r15
	call	"Agda.Builtin.Nat._+_"@PLT
	mov	r15, qword ptr [rsp + 56]
.LBB0_2:                                # %continue_0
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], r15
	mov	rdi, rbx
	call	drop@PLT
	cmp	qword ptr [r12 + 8], 0
	mov	rbx, qword ptr [r12 + 16]
	je	.LBB0_4
# %bb.3:                                # %FAgda.Builtin.Nat._-__1
	mov	rdx, qword ptr [r12 + 24]
	lea	rdi, [rsp + 8]
	mov	rsi, rbx
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rbx, qword ptr [rsp + 24]
.LBB0_4:                                # %continue_1
	mov	qword ptr [r12 + 8], 0
	mov	qword ptr [r12 + 16], rbx
	mov	rdi, r12
	call	drop@PLT
	add	r15, rbx
	mov	qword ptr [r14 + 16], r15
	mov	qword ptr [r14 + 8], 0
	mov	rax, r14
	add	rsp, 72
	pop	rbx
	pop	r12
	pop	r14
	pop	r15
	ret
.Lfunc_end0:
	.size	"Agda.Builtin.Nat._+_", .Lfunc_end0-"Agda.Builtin.Nat._+_"
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
	je	.LBB1_2
# %bb.1:                                # %FAgda.Builtin.Nat._-__2
	mov	rdx, qword ptr [rbx + 24]
	lea	rdi, [rsp + 8]
	mov	rsi, r12
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	r12, qword ptr [rsp + 24]
.LBB1_2:                                # %continue_2
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
.Lfunc_end1:
	.size	"Agda.Builtin.Nat._-_", .Lfunc_end1-"Agda.Builtin.Nat._-_"
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4                               # -- Begin function DownFromTail.downFrom
.LCPI2_0:
	.quad	1                               # 0x1
	.quad	0                               # 0x0
.LCPI2_1:
	.quad	2                               # 0x2
	.quad	2                               # 0x2
.LCPI2_2:
	.quad	1                               # 0x1
	.quad	4                               # 0x4
	.text
	.globl	DownFromTail.downFrom
	.p2align	4, 0x90
	.type	DownFromTail.downFrom,@function
DownFromTail.downFrom:                  # @DownFromTail.downFrom
# %bb.0:
	push	rbp
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	sub	rsp, 40
	mov	r13, rdx
	mov	r12, rsi
	mov	r14, rdi
	lea	r15, [rsp + 8]
	cmp	qword ptr [r13 + 8], 0
	mov	rsi, qword ptr [r13 + 16]
	je	.LBB2_3
	.p2align	4, 0x90
.LBB2_2:                                # %FAgda.Builtin.Nat._-__3
	mov	rdx, qword ptr [r13 + 24]
	mov	rdi, r15
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rsi, qword ptr [rsp + 24]
.LBB2_3:                                # %continue_3
                                        # =>This Inner Loop Header: Depth=1
	mov	qword ptr [r13 + 8], 0
	mov	qword ptr [r13 + 16], rsi
	test	rsi, rsi
	je	.LBB2_4
# %bb.11:                               # %default_4
                                        #   in Loop: Header=BB2_3 Depth=1
	mov	edi, 32
	call	malloc@PLT
	mov	rbx, rax
	movaps	xmm0, xmmword ptr [rip + .LCPI2_0] # xmm0 = [1,0]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], 1
	mov	edi, 32
	call	malloc@PLT
	mov	rbp, rax
	mov	qword ptr [rax + 16], r13
	mov	qword ptr [rax + 24], rbx
	movaps	xmm0, xmmword ptr [rip + .LCPI2_1] # xmm0 = [2,2]
	movups	xmmword ptr [rax], xmm0
	mov	edi, 32
	call	malloc@PLT
	movaps	xmm0, xmmword ptr [rip + .LCPI2_2] # xmm0 = [1,4]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], rbp
	mov	qword ptr [rax + 24], r12
	mov	r12, rax
	mov	r13, rbp
	cmp	qword ptr [r13 + 8], 0
	mov	rsi, qword ptr [r13 + 16]
	jne	.LBB2_2
	jmp	.LBB2_3
.LBB2_4:                                # %"0_4"
	mov	rdi, r13
	call	drop@PLT
	cmp	qword ptr [r12 + 8], 3
	jne	.LBB2_6
# %bb.5:
	mov	eax, 3
                                        # implicit-def: $rbx
                                        # implicit-def: $rbp
	cmp	rax, 3
	je	.LBB2_8
.LBB2_9:                                # %"CDownFromTail.List._\E2\88\B7__6"
	mov	qword ptr [r12 + 8], rax
	mov	qword ptr [r12 + 16], rbx
	mov	qword ptr [r12 + 24], rbp
	mov	rdi, r12
	call	drop@PLT
	inc	qword ptr [rbp]
	inc	qword ptr [rbx]
	mov	eax, 4
	jmp	.LBB2_10
.LBB2_6:                                # %"CDownFromTail.List._\E2\88\B7__5"
	mov	rbx, qword ptr [r12 + 16]
	mov	rbp, qword ptr [r12 + 24]
	inc	qword ptr [rbp]
	inc	qword ptr [rbx]
	mov	eax, 4
	cmp	rax, 3
	jne	.LBB2_9
.LBB2_8:                                # %"CDownFromTail.List.[]_6"
	mov	qword ptr [r12 + 8], 3
	mov	qword ptr [r12 + 16], rbx
	mov	qword ptr [r12 + 24], rbp
	mov	rdi, r12
	call	drop@PLT
	mov	eax, 3
                                        # implicit-def: $rbx
                                        # implicit-def: $rbp
.LBB2_10:                               # %common.ret
	mov	qword ptr [r14], rax
	mov	qword ptr [r14 + 8], rax
	mov	qword ptr [r14 + 16], rbx
	mov	qword ptr [r14 + 24], rbp
	mov	rax, r14
	add	rsp, 40
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	pop	rbp
	ret
.Lfunc_end2:
	.size	DownFromTail.downFrom, .Lfunc_end2-DownFromTail.downFrom
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4                               # -- Begin function DownFromTail.sum
.LCPI3_0:
	.quad	1                               # 0x1
	.quad	1                               # 0x1
	.text
	.globl	DownFromTail.sum
	.p2align	4, 0x90
	.type	DownFromTail.sum,@function
DownFromTail.sum:                       # @DownFromTail.sum
# %bb.0:
	push	rbp
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	sub	rsp, 72
	mov	rbx, rdx
	mov	r12, rsi
	mov	r14, rdi
	lea	r15, [rsp + 8]
	mov	rax, qword ptr [rbx + 8]
	cmp	rax, 3
	je	.LBB3_2
	.p2align	4, 0x90
.LBB3_3:                                # %tailrecurse
	cmp	rax, 5
	jne	.LBB3_4
# %bb.5:                                # %FDownFromTail.downFrom_7
	mov	rsi, qword ptr [rbx + 16]
	mov	rdx, qword ptr [rbx + 24]
	mov	rdi, r15
	call	DownFromTail.downFrom@PLT
	mov	r13, qword ptr [rsp + 32]
	mov	rbp, qword ptr [rsp + 24]
	mov	rax, qword ptr [rsp + 16]
	cmp	rax, 3
	jne	.LBB3_10
	jmp	.LBB3_7
	.p2align	4, 0x90
.LBB3_2:
	mov	eax, 3
                                        # implicit-def: $rbp
                                        # implicit-def: $r13
	cmp	rax, 3
	jne	.LBB3_10
	jmp	.LBB3_7
	.p2align	4, 0x90
.LBB3_4:                                # %"CDownFromTail.List._\E2\88\B7__7"
	mov	rbp, qword ptr [rbx + 16]
	mov	r13, qword ptr [rbx + 24]
	inc	qword ptr [r13]
	inc	qword ptr [rbp]
	mov	eax, 4
	cmp	rax, 3
	je	.LBB3_7
.LBB3_10:                               # %"CDownFromTail.List._\E2\88\B7__8"
	mov	qword ptr [rbx + 8], rax
	mov	qword ptr [rbx + 16], rbp
	mov	qword ptr [rbx + 24], r13
	mov	rdi, rbx
	call	drop@PLT
	mov	edi, 32
	call	malloc@PLT
	movaps	xmm0, xmmword ptr [rip + .LCPI3_0] # xmm0 = [1,1]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], r12
	mov	qword ptr [rax + 24], rbp
	mov	r12, rax
	mov	rbx, r13
	mov	rax, qword ptr [rbx + 8]
	cmp	rax, 3
	jne	.LBB3_3
	jmp	.LBB3_2
.LBB3_7:                                # %"CDownFromTail.List.[]_8"
	mov	qword ptr [rbx + 8], 3
	mov	qword ptr [rbx + 16], rbp
	mov	qword ptr [rbx + 24], r13
	mov	rdi, rbx
	call	drop@PLT
	cmp	qword ptr [r12 + 8], 0
	mov	rbx, qword ptr [r12 + 16]
	je	.LBB3_9
# %bb.8:                                # %"FAgda.Builtin.Nat._+__9"
	mov	rdx, qword ptr [r12 + 24]
	lea	rdi, [rsp + 40]
	mov	rsi, rbx
	call	"Agda.Builtin.Nat._+_"@PLT
	mov	rbx, qword ptr [rsp + 56]
.LBB3_9:                                # %continue_9
	mov	qword ptr [r12 + 8], 0
	mov	qword ptr [r12 + 16], rbx
	mov	rdi, r12
	call	drop@PLT
	mov	qword ptr [r14 + 16], rbx
	mov	qword ptr [r14 + 8], 0
	mov	rax, r14
	add	rsp, 72
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	pop	rbp
	ret
.Lfunc_end3:
	.size	DownFromTail.sum, .Lfunc_end3-DownFromTail.sum
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4                               # -- Begin function main
.LCPI4_0:
	.quad	1                               # 0x1
	.quad	3                               # 0x3
.LCPI4_1:
	.quad	1                               # 0x1
	.quad	0                               # 0x0
.LCPI4_2:
	.quad	1                               # 0x1
	.quad	5                               # 0x5
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	push	r15
	.cfi_def_cfa_offset 16
	push	r14
	.cfi_def_cfa_offset 24
	push	rbx
	.cfi_def_cfa_offset 32
	sub	rsp, 32
	.cfi_def_cfa_offset 64
	.cfi_offset rbx, -32
	.cfi_offset r14, -24
	.cfi_offset r15, -16
	mov	edi, 32
	call	malloc@PLT
	mov	r15, rax
	mov	qword ptr [rax], 1
	mov	edi, 32
	call	malloc@PLT
	mov	r14, rax
	xorps	xmm0, xmm0
	movups	xmmword ptr [r15 + 8], xmm0
	movaps	xmm0, xmmword ptr [rip + .LCPI4_0] # xmm0 = [1,3]
	movups	xmmword ptr [rax], xmm0
	mov	edi, 32
	call	malloc@PLT
	mov	rbx, rax
	movaps	xmm0, xmmword ptr [rip + .LCPI4_1] # xmm0 = [1,0]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], 100
	mov	edi, 32
	call	malloc@PLT
	movaps	xmm0, xmmword ptr [rip + .LCPI4_2] # xmm0 = [1,5]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], r14
	mov	qword ptr [rax + 24], rbx
	mov	rdi, rsp
	mov	rsi, r15
	mov	rdx, rax
	call	DownFromTail.sum@PLT
	mov	rsi, qword ptr [rsp + 16]
	lea	rdi, [rip + ".L%d"]
	add	rsp, 32
	.cfi_def_cfa_offset 32
	pop	rbx
	.cfi_def_cfa_offset 24
	pop	r14
	.cfi_def_cfa_offset 16
	pop	r15
	.cfi_def_cfa_offset 8
	jmp	printf@PLT                      # TAILCALL
.Lfunc_end4:
	.size	main, .Lfunc_end4-main
	.cfi_endproc
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
	jne	.LBB5_4
# %bb.1:                                # %"1_10"
	mov	rax, qword ptr [rbx + 8]
	lea	rcx, [rip + .LJTI5_0]
	movsxd	rax, dword ptr [rcx + 4*rax]
	add	rax, rcx
	jmp	rax
.LBB5_2:                                # %"CDownFromTail.List._\E2\88\B7__11"
	mov	rdi, qword ptr [rbx + 16]
	call	drop@PLT
	mov	rdi, qword ptr [rbx + 24]
	call	drop@PLT
.LBB5_3:                                # %"CDownFromTail.List.[]_11"
	mov	rdi, rbx
	pop	rbx
	jmp	free@PLT                        # TAILCALL
.LBB5_4:                                # %default_10
	dec	rax
	mov	qword ptr [rbx], rax
	pop	rbx
	ret
.Lfunc_end5:
	.size	drop, .Lfunc_end5-drop
	.section	.rodata,"a",@progbits
	.p2align	2
.LJTI5_0:
	.long	.LBB5_3-.LJTI5_0
	.long	.LBB5_2-.LJTI5_0
	.long	.LBB5_2-.LJTI5_0
	.long	.LBB5_3-.LJTI5_0
	.long	.LBB5_2-.LJTI5_0
	.long	.LBB5_2-.LJTI5_0
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
