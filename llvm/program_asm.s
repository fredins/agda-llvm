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
	sub	rsp, 40
	mov	r12, rdx
	mov	rbx, rsi
	mov	r14, rdi
	cmp	qword ptr [rsi + 8], 0
	mov	r15, qword ptr [rsi + 16]
	je	.LBB0_2
# %bb.1:                                # %FAgda.Builtin.Nat._-__0
	mov	rdx, qword ptr [rbx + 24]
	lea	rdi, [rsp + 8]
	mov	rsi, r15
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	r15, qword ptr [rsp + 24]
.LBB0_2:                                # %continue_0
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], r15
	mov	rax, qword ptr [rbx]
	cmp	rax, 1
	jne	.LBB0_4
# %bb.3:                                # %"1_2"
	mov	rdi, rbx
	call	free@PLT
	jmp	.LBB0_5
.LBB0_4:                                # %default_2
	dec	rax
	mov	qword ptr [rbx], rax
.LBB0_5:                                # %continue_1
	mov	rax, qword ptr [r12]
	mov	rbx, qword ptr [r12 + 16]
	mov	qword ptr [r12 + 8], 0
	cmp	rax, 1
	jne	.LBB0_7
# %bb.6:                                # %"1_4"
	mov	rdi, r12
	call	free@PLT
	jmp	.LBB0_8
.LBB0_7:                                # %default_4
	dec	rax
	mov	qword ptr [r12], rax
.LBB0_8:                                # %continue_3
	add	rbx, r15
	mov	qword ptr [r14 + 16], rbx
	mov	qword ptr [r14 + 8], 0
	mov	rax, r14
	add	rsp, 40
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
# %bb.1:                                # %FAgda.Builtin.Nat._-__5
	mov	rdx, qword ptr [rbx + 24]
	lea	rdi, [rsp + 8]
	mov	rsi, r12
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	r12, qword ptr [rsp + 24]
.LBB1_2:                                # %continue_5
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], r12
	mov	rax, qword ptr [rbx]
	cmp	rax, 1
	jne	.LBB1_4
# %bb.3:                                # %"1_7"
	mov	rdi, rbx
	call	free@PLT
	jmp	.LBB1_5
.LBB1_4:                                # %default_7
	dec	rax
	mov	qword ptr [rbx], rax
.LBB1_5:                                # %continue_6
	mov	rax, qword ptr [r15]
	mov	rbx, qword ptr [r15 + 16]
	mov	qword ptr [r15 + 8], 0
	cmp	rax, 1
	jne	.LBB1_7
# %bb.6:                                # %"1_9"
	mov	rdi, r15
	call	free@PLT
	jmp	.LBB1_8
.LBB1_7:                                # %default_9
	dec	rax
	mov	qword ptr [r15], rax
.LBB1_8:                                # %continue_8
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
.Lfunc_end1:
	.size	"Agda.Builtin.Nat._-_", .Lfunc_end1-"Agda.Builtin.Nat._-_"
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4                               # -- Begin function DownFromOpt.downFrom
.LCPI2_0:
	.quad	1                               # 0x1
	.quad	0                               # 0x0
.LCPI2_1:
	.quad	2                               # 0x2
	.quad	1                               # 0x1
.LCPI2_2:
	.quad	1                               # 0x1
	.quad	3                               # 0x3
	.text
	.globl	DownFromOpt.downFrom
	.p2align	4, 0x90
	.type	DownFromOpt.downFrom,@function
DownFromOpt.downFrom:                   # @DownFromOpt.downFrom
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
	je	.LBB2_2
# %bb.1:                                # %FAgda.Builtin.Nat._-__10
	mov	rdx, qword ptr [rbx + 24]
	lea	rdi, [rsp + 8]
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rsi, qword ptr [rsp + 24]
.LBB2_2:                                # %continue_10
	mov	qword ptr [rbx + 8], 0
	mov	qword ptr [rbx + 16], rsi
	test	rsi, rsi
	je	.LBB2_3
# %bb.8:                                # %default_11
	mov	edi, 32
	call	malloc@PLT
	mov	r12, rax
	movaps	xmm0, xmmword ptr [rip + .LCPI2_0] # xmm0 = [1,0]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], 1
	mov	edi, 32
	call	malloc@PLT
	mov	r15, rax
	mov	qword ptr [rax + 16], rbx
	mov	qword ptr [rax + 24], r12
	movaps	xmm0, xmmword ptr [rip + .LCPI2_1] # xmm0 = [2,1]
	movups	xmmword ptr [rax], xmm0
	mov	edi, 32
	call	malloc@PLT
	movaps	xmm0, xmmword ptr [rip + .LCPI2_2] # xmm0 = [1,3]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], r15
	mov	ecx, 4
	jmp	.LBB2_7
.LBB2_3:                                # %"0_11"
	mov	rax, qword ptr [rbx]
	cmp	rax, 1
	jne	.LBB2_5
# %bb.4:                                # %"1_13"
	mov	rdi, rbx
	call	free@PLT
	jmp	.LBB2_6
.LBB2_5:                                # %default_13
	dec	rax
	mov	qword ptr [rbx], rax
.LBB2_6:                                # %common.ret
	mov	ecx, 2
                                        # implicit-def: $r15
                                        # implicit-def: $rax
.LBB2_7:                                # %common.ret
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
.Lfunc_end2:
	.size	DownFromOpt.downFrom, .Lfunc_end2-DownFromOpt.downFrom
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4                               # -- Begin function DownFromOpt.sum
.LCPI3_0:
	.long	1                               # 0x1
	.long	0                               # 0x0
	.long	0                               # 0x0
	.long	0                               # 0x0
	.text
	.globl	DownFromOpt.sum
	.p2align	4, 0x90
	.type	DownFromOpt.sum,@function
DownFromOpt.sum:                        # @DownFromOpt.sum
# %bb.0:
	push	rbp
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	sub	rsp, 72
	mov	rbx, rdx
	mov	r13, rsi
	mov	qword ptr [rsp], rdi            # 8-byte Spill
	lea	r15, [rsp + 40]
	lea	r12, [rsp + 8]
	jmp	.LBB3_1
	.p2align	4, 0x90
.LBB3_24:                               # %default_4.i
                                        #   in Loop: Header=BB3_1 Depth=1
	dec	rax
	mov	qword ptr [r13], rax
.LBB3_25:                               # %"Agda.Builtin.Nat._+_.exit"
                                        #   in Loop: Header=BB3_1 Depth=1
	add	rbp, rbx
	mov	edi, 32
	call	malloc@PLT
	mov	r13, rax
	movaps	xmm0, xmmword ptr [rip + .LCPI3_0] # xmm0 = [1,0,0,0]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], rbp
	mov	rbx, r14
.LBB3_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	mov	rax, qword ptr [rbx + 8]
	cmp	rax, 2
	je	.LBB3_2
# %bb.3:                                # %tailrecurse
                                        #   in Loop: Header=BB3_1 Depth=1
	cmp	rax, 3
	jne	.LBB3_4
# %bb.5:                                # %FDownFromOpt.downFrom_14
                                        #   in Loop: Header=BB3_1 Depth=1
	mov	rsi, qword ptr [rbx + 16]
	mov	rdi, r12
	call	DownFromOpt.downFrom@PLT
	mov	r14, qword ptr [rsp + 32]
	mov	rbp, qword ptr [rsp + 24]
	mov	rax, qword ptr [rsp + 16]
	cmp	rax, 2
	jne	.LBB3_14
	jmp	.LBB3_7
	.p2align	4, 0x90
.LBB3_2:                                #   in Loop: Header=BB3_1 Depth=1
	mov	eax, 2
                                        # implicit-def: $rbp
                                        # implicit-def: $r14
	cmp	rax, 2
	jne	.LBB3_14
	jmp	.LBB3_7
	.p2align	4, 0x90
.LBB3_4:                                # %"CDownFromOpt.List._\E2\88\B7__14"
                                        #   in Loop: Header=BB3_1 Depth=1
	mov	rbp, qword ptr [rbx + 16]
	mov	r14, qword ptr [rbx + 24]
	mov	eax, 4
	cmp	rax, 2
	je	.LBB3_7
.LBB3_14:                               # %"CDownFromOpt.List._\E2\88\B7__15"
                                        #   in Loop: Header=BB3_1 Depth=1
	mov	qword ptr [rbx + 8], 4
	mov	qword ptr [rbx + 16], rbp
	mov	qword ptr [rbx + 24], r14
	mov	rax, qword ptr [rbx]
	cmp	rax, 1
	jne	.LBB3_16
# %bb.15:                               # %"1_21"
                                        #   in Loop: Header=BB3_1 Depth=1
	mov	rdi, rbx
	call	free@PLT
	cmp	qword ptr [rbp + 8], 0
	mov	rbx, qword ptr [rbp + 16]
	jne	.LBB3_18
	jmp	.LBB3_19
	.p2align	4, 0x90
.LBB3_16:                               # %default_21
                                        #   in Loop: Header=BB3_1 Depth=1
	inc	qword ptr [r14]
	inc	qword ptr [rbp]
	dec	rax
	mov	qword ptr [rbx], rax
	cmp	qword ptr [rbp + 8], 0
	mov	rbx, qword ptr [rbp + 16]
	je	.LBB3_19
.LBB3_18:                               # %FAgda.Builtin.Nat._-__0.i
                                        #   in Loop: Header=BB3_1 Depth=1
	mov	rdx, qword ptr [rbp + 24]
	mov	rdi, r15
	mov	rsi, rbx
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rbx, qword ptr [rsp + 56]
.LBB3_19:                               # %continue_0.i
                                        #   in Loop: Header=BB3_1 Depth=1
	mov	qword ptr [rbp + 8], 0
	mov	qword ptr [rbp + 16], rbx
	mov	rax, qword ptr [rbp]
	cmp	rax, 1
	jne	.LBB3_21
# %bb.20:                               # %"1_2.i"
                                        #   in Loop: Header=BB3_1 Depth=1
	mov	rdi, rbp
	call	free@PLT
	jmp	.LBB3_22
	.p2align	4, 0x90
.LBB3_21:                               # %default_2.i
                                        #   in Loop: Header=BB3_1 Depth=1
	dec	rax
	mov	qword ptr [rbp], rax
.LBB3_22:                               # %continue_1.i
                                        #   in Loop: Header=BB3_1 Depth=1
	mov	rax, qword ptr [r13]
	mov	rbp, qword ptr [r13 + 16]
	mov	qword ptr [r13 + 8], 0
	cmp	rax, 1
	jne	.LBB3_24
# %bb.23:                               # %"1_4.i"
                                        #   in Loop: Header=BB3_1 Depth=1
	mov	rdi, r13
	call	free@PLT
	jmp	.LBB3_25
.LBB3_7:                                # %"CDownFromOpt.List.[]_15"
	mov	qword ptr [rbx + 8], 2
	mov	rax, qword ptr [rbx]
	cmp	rax, 1
	jne	.LBB3_9
# %bb.8:                                # %"1_17"
	mov	rdi, rbx
	call	free@PLT
	jmp	.LBB3_10
.LBB3_9:                                # %default_17
	dec	rax
	mov	qword ptr [rbx], rax
.LBB3_10:                               # %continue_16
	mov	rax, qword ptr [r13]
	mov	rbx, qword ptr [r13 + 16]
	mov	qword ptr [r13 + 8], 0
	cmp	rax, 1
	jne	.LBB3_12
# %bb.11:                               # %"1_19"
	mov	rdi, r13
	call	free@PLT
	jmp	.LBB3_13
.LBB3_12:                               # %default_19
	dec	rax
	mov	qword ptr [r13], rax
.LBB3_13:                               # %continue_18
	mov	rax, qword ptr [rsp]            # 8-byte Reload
	mov	qword ptr [rax + 16], rbx
	mov	qword ptr [rax + 8], 0
	add	rsp, 72
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	pop	rbp
	ret
.Lfunc_end3:
	.size	DownFromOpt.sum, .Lfunc_end3-DownFromOpt.sum
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4                               # -- Begin function main
.LCPI4_0:
	.quad	1                               # 0x1
	.quad	0                               # 0x0
.LCPI4_1:
	.quad	1                               # 0x1
	.quad	3                               # 0x3
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	push	r14
	.cfi_def_cfa_offset 16
	push	rbx
	.cfi_def_cfa_offset 24
	sub	rsp, 40
	.cfi_def_cfa_offset 64
	.cfi_offset rbx, -24
	.cfi_offset r14, -16
	mov	edi, 32
	call	malloc@PLT
	mov	r14, rax
	mov	qword ptr [rax], 1
	mov	edi, 32
	call	malloc@PLT
	mov	rbx, rax
	xorps	xmm0, xmm0
	movups	xmmword ptr [r14 + 8], xmm0
	movaps	xmm0, xmmword ptr [rip + .LCPI4_0] # xmm0 = [1,0]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], 100
	mov	edi, 32
	call	malloc@PLT
	movaps	xmm0, xmmword ptr [rip + .LCPI4_1] # xmm0 = [1,3]
	movups	xmmword ptr [rax], xmm0
	mov	qword ptr [rax + 16], rbx
	lea	rdi, [rsp + 8]
	mov	rsi, r14
	mov	rdx, rax
	call	DownFromOpt.sum@PLT
	mov	rsi, qword ptr [rsp + 24]
	lea	rdi, [rip + ".L%d"]
	add	rsp, 40
	.cfi_def_cfa_offset 24
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	r14
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
	jne	.LBB5_6
# %bb.1:                                # %"1_22"
	mov	rax, qword ptr [rbx + 8]
	lea	rcx, [rip + .LJTI5_0]
	movsxd	rax, dword ptr [rcx + 4*rax]
	add	rax, rcx
	jmp	rax
.LBB5_2:                                # %"CDownFromOpt.List._\E2\88\B7__23"
	mov	rdi, qword ptr [rbx + 16]
	call	drop@PLT
	mov	rdi, qword ptr [rbx + 24]
	jmp	.LBB5_4
.LBB5_6:                                # %default_22
	dec	rax
	mov	qword ptr [rbx], rax
	pop	rbx
	ret
.LBB5_3:                                # %FDownFromOpt.downFrom_23
	mov	rdi, qword ptr [rbx + 16]
.LBB5_4:                                # %FDownFromOpt.downFrom_23
	call	drop@PLT
.LBB5_5:                                # %"CDownFromOpt.List.[]_23"
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
