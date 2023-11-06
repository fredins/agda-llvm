	.text
	.intel_syntax noprefix
	.file	"program.ll"
	.globl	"Agda.Builtin.Nat._+_"          # -- Begin function Agda.Builtin.Nat._+_
	.p2align	4, 0x90
	.type	"Agda.Builtin.Nat._+_",@function
"Agda.Builtin.Nat._+_":                 # @"Agda.Builtin.Nat._+_"
# %bb.0:
	sub	rsp, 152
	mov	qword ptr [rsp + 40], rdx       # 8-byte Spill
	mov	qword ptr [rsp + 64], rsi       # 8-byte Spill
	mov	rax, rdi
	mov	qword ptr [rsp + 48], rax       # 8-byte Spill
	mov	qword ptr [rsp + 56], rdi       # 8-byte Spill
	mov	rsi, qword ptr [rsi + 16]
	lea	rdi, [rsp + 120]
	call	Upto.sum@PLT
	mov	rsi, qword ptr [rsp + 64]       # 8-byte Reload
	mov	rax, qword ptr [rsp + 144]
	mov	rax, qword ptr [rsp + 136]
	mov	qword ptr [rsp + 72], rax       # 8-byte Spill
	mov	rcx, qword ptr [rsp + 120]
	mov	rcx, qword ptr [rsp + 128]
	mov	qword ptr [rsi + 8], 0
	mov	qword ptr [rsi + 16], rax
	mov	rax, qword ptr [rsi]
	mov	qword ptr [rsp + 80], rax       # 8-byte Spill
	cmp	rax, 1
	jne	.LBB0_2
# %bb.1:                                # %"1_1"
	mov	rdi, qword ptr [rsp + 64]       # 8-byte Reload
	call	free@PLT
	jmp	.LBB0_3
.LBB0_2:                                # %default_1
	mov	rax, qword ptr [rsp + 64]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 80]       # 8-byte Reload
	add	rcx, -1
	mov	qword ptr [rax], rcx
.LBB0_3:                                # %continue_0
	mov	rax, qword ptr [rsp + 40]       # 8-byte Reload
	mov	rcx, rax
	add	rcx, 8
	mov	qword ptr [rsp + 16], rcx       # 8-byte Spill
	mov	rcx, qword ptr [rax + 8]
	mov	rax, qword ptr [rax + 16]
	mov	qword ptr [rsp + 24], rax       # 8-byte Spill
	cmp	rcx, 0
	mov	qword ptr [rsp + 32], rax       # 8-byte Spill
	je	.LBB0_5
# %bb.4:                                # %FAgda.Builtin.Nat._-__2
	mov	rsi, qword ptr [rsp + 24]       # 8-byte Reload
	mov	rax, qword ptr [rsp + 40]       # 8-byte Reload
	mov	rdx, qword ptr [rax + 24]
	lea	rdi, [rsp + 88]
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rax, qword ptr [rsp + 112]
	mov	rax, qword ptr [rsp + 104]
	mov	rcx, qword ptr [rsp + 88]
	mov	rcx, qword ptr [rsp + 96]
	mov	qword ptr [rsp + 32], rax       # 8-byte Spill
.LBB0_5:                                # %continue_2
	mov	rax, qword ptr [rsp + 40]       # 8-byte Reload
	mov	rdx, qword ptr [rsp + 16]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 32]       # 8-byte Reload
	mov	qword ptr [rsp], rcx            # 8-byte Spill
	mov	qword ptr [rdx], 0
	mov	qword ptr [rax + 16], rcx
	mov	rax, qword ptr [rax]
	mov	qword ptr [rsp + 8], rax        # 8-byte Spill
	cmp	rax, 1
	jne	.LBB0_7
# %bb.6:                                # %"1_4"
	mov	rdi, qword ptr [rsp + 40]       # 8-byte Reload
	call	free@PLT
	jmp	.LBB0_8
.LBB0_7:                                # %default_4
	mov	rax, qword ptr [rsp + 40]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 8]        # 8-byte Reload
	add	rcx, -1
	mov	qword ptr [rax], rcx
.LBB0_8:                                # %continue_3
	mov	rax, qword ptr [rsp + 48]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 56]       # 8-byte Reload
	mov	rsi, qword ptr [rsp + 72]       # 8-byte Reload
	mov	rdx, qword ptr [rsp]            # 8-byte Reload
	add	rdx, rsi
	mov	qword ptr [rcx + 16], rdx
	mov	qword ptr [rcx + 8], 0
	add	rsp, 152
	ret
.Lfunc_end0:
	.size	"Agda.Builtin.Nat._+_", .Lfunc_end0-"Agda.Builtin.Nat._+_"
                                        # -- End function
	.globl	"Agda.Builtin.Nat._-_"          # -- Begin function Agda.Builtin.Nat._-_
	.p2align	4, 0x90
	.type	"Agda.Builtin.Nat._-_",@function
"Agda.Builtin.Nat._-_":                 # @Agda.Builtin.Nat._-_
# %bb.0:
	sub	rsp, 120
	mov	qword ptr [rsp + 32], rdx       # 8-byte Spill
	mov	qword ptr [rsp + 40], rsi       # 8-byte Spill
	mov	rax, rdi
	mov	qword ptr [rsp + 48], rax       # 8-byte Spill
	mov	qword ptr [rsp + 56], rdi       # 8-byte Spill
	mov	rax, rsi
	add	rax, 8
	mov	qword ptr [rsp + 64], rax       # 8-byte Spill
	mov	rcx, qword ptr [rsi + 8]
	mov	rax, qword ptr [rsi + 16]
	mov	qword ptr [rsp + 72], rax       # 8-byte Spill
	cmp	rcx, 0
	mov	qword ptr [rsp + 80], rax       # 8-byte Spill
	je	.LBB1_2
# %bb.1:                                # %FAgda.Builtin.Nat._-__5
	mov	rsi, qword ptr [rsp + 72]       # 8-byte Reload
	mov	rax, qword ptr [rsp + 40]       # 8-byte Reload
	mov	rdx, qword ptr [rax + 24]
	lea	rdi, [rsp + 88]
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rax, qword ptr [rsp + 112]
	mov	rax, qword ptr [rsp + 104]
	mov	rcx, qword ptr [rsp + 88]
	mov	rcx, qword ptr [rsp + 96]
	mov	qword ptr [rsp + 80], rax       # 8-byte Spill
.LBB1_2:                                # %continue_5
	mov	rax, qword ptr [rsp + 40]       # 8-byte Reload
	mov	rdx, qword ptr [rsp + 64]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 80]       # 8-byte Reload
	mov	qword ptr [rsp + 16], rcx       # 8-byte Spill
	mov	qword ptr [rdx], 0
	mov	qword ptr [rax + 16], rcx
	mov	rax, qword ptr [rax]
	mov	qword ptr [rsp + 24], rax       # 8-byte Spill
	cmp	rax, 1
	jne	.LBB1_4
# %bb.3:                                # %"1_7"
	mov	rdi, qword ptr [rsp + 40]       # 8-byte Reload
	call	free@PLT
	jmp	.LBB1_5
.LBB1_4:                                # %default_7
	mov	rax, qword ptr [rsp + 40]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 24]       # 8-byte Reload
	add	rcx, -1
	mov	qword ptr [rax], rcx
.LBB1_5:                                # %continue_6
	mov	rax, qword ptr [rsp + 32]       # 8-byte Reload
	mov	rcx, qword ptr [rax + 16]
	mov	qword ptr [rsp], rcx            # 8-byte Spill
	mov	qword ptr [rax + 8], 0
	mov	rax, qword ptr [rax]
	mov	qword ptr [rsp + 8], rax        # 8-byte Spill
	cmp	rax, 1
	jne	.LBB1_7
# %bb.6:                                # %"1_9"
	mov	rdi, qword ptr [rsp + 32]       # 8-byte Reload
	call	free@PLT
	jmp	.LBB1_8
.LBB1_7:                                # %default_9
	mov	rax, qword ptr [rsp + 32]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 8]        # 8-byte Reload
	add	rcx, -1
	mov	qword ptr [rax], rcx
.LBB1_8:                                # %continue_8
	mov	rax, qword ptr [rsp + 48]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 56]       # 8-byte Reload
	mov	rsi, qword ptr [rsp]            # 8-byte Reload
	mov	rdx, qword ptr [rsp + 16]       # 8-byte Reload
	sub	rdx, rsi
	mov	qword ptr [rcx + 16], rdx
	mov	qword ptr [rcx + 8], 0
	add	rsp, 120
	ret
.Lfunc_end1:
	.size	"Agda.Builtin.Nat._-_", .Lfunc_end1-"Agda.Builtin.Nat._-_"
                                        # -- End function
	.globl	Upto.upTo                       # -- Begin function Upto.upTo
	.p2align	4, 0x90
	.type	Upto.upTo,@function
Upto.upTo:                              # @Upto.upTo
# %bb.0:
	sub	rsp, 248
	mov	rax, rdi
	mov	qword ptr [rsp + 184], rax      # 8-byte Spill
	mov	qword ptr [rsp + 192], rdi      # 8-byte Spill
	mov	qword ptr [rsp + 200], rsi      # 8-byte Spill
	mov	qword ptr [rsp + 208], rdx      # 8-byte Spill
	jmp	.LBB2_1
.LBB2_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	mov	rcx, qword ptr [rsp + 200]      # 8-byte Reload
	mov	rax, qword ptr [rsp + 208]      # 8-byte Reload
	mov	qword ptr [rsp + 144], rax      # 8-byte Spill
	mov	qword ptr [rsp + 152], rcx      # 8-byte Spill
	mov	rcx, rax
	add	rcx, 8
	mov	qword ptr [rsp + 160], rcx      # 8-byte Spill
	mov	rcx, qword ptr [rax + 8]
	mov	rax, qword ptr [rax + 16]
	mov	qword ptr [rsp + 168], rax      # 8-byte Spill
	cmp	rcx, 0
	mov	qword ptr [rsp + 176], rax      # 8-byte Spill
	je	.LBB2_3
# %bb.2:                                # %FAgda.Builtin.Nat._-__10
                                        #   in Loop: Header=BB2_1 Depth=1
	mov	rsi, qword ptr [rsp + 168]      # 8-byte Reload
	mov	rax, qword ptr [rsp + 144]      # 8-byte Reload
	mov	rdx, qword ptr [rax + 24]
	lea	rdi, [rsp + 216]
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rax, qword ptr [rsp + 240]
	mov	rax, qword ptr [rsp + 232]
	mov	rcx, qword ptr [rsp + 216]
	mov	rcx, qword ptr [rsp + 224]
	mov	qword ptr [rsp + 176], rax      # 8-byte Spill
.LBB2_3:                                # %continue_10
                                        #   in Loop: Header=BB2_1 Depth=1
	mov	rcx, qword ptr [rsp + 144]      # 8-byte Reload
	mov	rdx, qword ptr [rsp + 160]      # 8-byte Reload
	mov	rax, qword ptr [rsp + 176]      # 8-byte Reload
	mov	qword ptr [rdx], 0
	mov	qword ptr [rcx + 16], rax
	cmp	rax, 0
	jne	.LBB2_17
# %bb.4:                                # %"0_11"
	mov	rax, qword ptr [rsp + 144]      # 8-byte Reload
	mov	rax, qword ptr [rax]
	mov	qword ptr [rsp + 136], rax      # 8-byte Spill
	cmp	rax, 1
	jne	.LBB2_6
# %bb.5:                                # %"1_13"
	mov	rdi, qword ptr [rsp + 144]      # 8-byte Reload
	call	free@PLT
	jmp	.LBB2_7
.LBB2_6:                                # %default_13
	mov	rax, qword ptr [rsp + 144]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 136]      # 8-byte Reload
	add	rcx, -1
	mov	qword ptr [rax], rcx
.LBB2_7:                                # %continue_12
	mov	rax, qword ptr [rsp + 152]      # 8-byte Reload
	mov	rcx, rax
	mov	qword ptr [rsp + 112], rcx      # 8-byte Spill
	mov	rcx, rax
	add	rcx, 8
	mov	qword ptr [rsp + 120], rcx      # 8-byte Spill
	mov	rcx, qword ptr [rax + 8]
	mov	eax, 2
                                        # implicit-def: $rdx
	sub	rcx, 2
	mov	qword ptr [rsp + 128], rax      # 8-byte Spill
                                        # implicit-def: $rax
                                        # implicit-def: $rax
	je	.LBB2_9
	jmp	.LBB2_8
.LBB2_8:                                # %"CUpto.List._\E2\88\B7__14"
	mov	rax, qword ptr [rsp + 112]      # 8-byte Reload
	mov	rcx, qword ptr [rax + 16]
	mov	rax, qword ptr [rax + 24]
	mov	edx, 3
                                        # implicit-def: $rsi
	mov	qword ptr [rsp + 128], rdx      # 8-byte Spill
	mov	qword ptr [rsp + 96], rcx       # 8-byte Spill
	mov	qword ptr [rsp + 104], rax      # 8-byte Spill
	jmp	.LBB2_9
.LBB2_9:                                # %continue_14
	mov	rax, qword ptr [rsp + 128]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 96]       # 8-byte Reload
	mov	rdx, qword ptr [rsp + 104]      # 8-byte Reload
	mov	qword ptr [rsp + 80], rdx       # 8-byte Spill
	mov	qword ptr [rsp + 88], rcx       # 8-byte Spill
                                        # implicit-def: $rcx
	cmp	rax, 2
	jne	.LBB2_14
# %bb.10:                               # %"CUpto.List.[]_15"
	mov	rax, qword ptr [rsp + 112]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 120]      # 8-byte Reload
	mov	qword ptr [rcx], 2
	mov	rax, qword ptr [rax]
	mov	qword ptr [rsp + 72], rax       # 8-byte Spill
	cmp	rax, 1
	jne	.LBB2_12
# %bb.11:                               # %"1_17"
	mov	rdi, qword ptr [rsp + 112]      # 8-byte Reload
	call	free@PLT
	mov	eax, 2
                                        # implicit-def: $rcx
	mov	qword ptr [rsp + 64], rax       # 8-byte Spill
                                        # implicit-def: $rax
                                        # implicit-def: $rax
	jmp	.LBB2_13
.LBB2_12:                               # %default_17
	mov	rax, qword ptr [rsp + 112]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 72]       # 8-byte Reload
	dec	rcx
	mov	qword ptr [rax], rcx
	mov	eax, 2
                                        # implicit-def: $rcx
	mov	qword ptr [rsp + 64], rax       # 8-byte Spill
                                        # implicit-def: $rax
                                        # implicit-def: $rax
	jmp	.LBB2_13
.LBB2_13:                               # %common.ret
	mov	rax, qword ptr [rsp + 184]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 192]      # 8-byte Reload
	mov	rdi, qword ptr [rsp + 64]       # 8-byte Reload
	mov	rsi, qword ptr [rsp + 56]       # 8-byte Reload
	mov	rdx, qword ptr [rsp + 48]       # 8-byte Reload
                                        # implicit-def: $r8
	mov	qword ptr [rcx], r8
	mov	qword ptr [rcx + 8], rdi
	mov	qword ptr [rcx + 16], rsi
	mov	qword ptr [rcx + 24], rdx
	add	rsp, 248
	ret
.LBB2_14:                               # %"CUpto.List._\E2\88\B7__15"
	mov	rax, qword ptr [rsp + 112]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 80]       # 8-byte Reload
	mov	rdx, qword ptr [rsp + 88]       # 8-byte Reload
	mov	rsi, qword ptr [rsp + 120]      # 8-byte Reload
	mov	rdi, rcx
	mov	qword ptr [rsp + 16], rdi       # 8-byte Spill
	mov	rdi, rdx
	mov	qword ptr [rsp + 24], rdi       # 8-byte Spill
	mov	edi, 3
	mov	qword ptr [rsp + 32], rdi       # 8-byte Spill
                                        # implicit-def: $rdi
	mov	qword ptr [rsi], 3
	mov	qword ptr [rax + 16], rdx
	mov	qword ptr [rax + 24], rcx
	mov	rax, qword ptr [rax]
	mov	qword ptr [rsp + 40], rax       # 8-byte Spill
	cmp	rax, 1
	jne	.LBB2_16
# %bb.15:                               # %"1_19"
	mov	rdi, qword ptr [rsp + 112]      # 8-byte Reload
	call	free@PLT
	mov	rdx, qword ptr [rsp + 32]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 24]       # 8-byte Reload
	mov	rax, qword ptr [rsp + 16]       # 8-byte Reload
	mov	qword ptr [rsp + 64], rdx       # 8-byte Spill
	mov	qword ptr [rsp + 56], rcx       # 8-byte Spill
	mov	qword ptr [rsp + 48], rax       # 8-byte Spill
	jmp	.LBB2_13
.LBB2_16:                               # %default_19
	mov	rax, qword ptr [rsp + 16]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 24]       # 8-byte Reload
	mov	rdx, qword ptr [rsp + 32]       # 8-byte Reload
	mov	rsi, qword ptr [rsp + 112]      # 8-byte Reload
	mov	rdi, qword ptr [rsp + 40]       # 8-byte Reload
	mov	r8, qword ptr [rsp + 88]        # 8-byte Reload
	mov	r9, qword ptr [rsp + 80]        # 8-byte Reload
	mov	r10, qword ptr [r9]
	inc	r10
	mov	qword ptr [r9], r10
	mov	r9, qword ptr [r8]
	inc	r9
	mov	qword ptr [r8], r9
	dec	rdi
	mov	qword ptr [rsi], rdi
	mov	qword ptr [rsp + 64], rdx       # 8-byte Spill
	mov	qword ptr [rsp + 56], rcx       # 8-byte Spill
	mov	qword ptr [rsp + 48], rax       # 8-byte Spill
	jmp	.LBB2_13
.LBB2_17:                               # %default_11
                                        #   in Loop: Header=BB2_1 Depth=1
	mov	edi, 32
	call	malloc@PLT
	mov	qword ptr [rsp], rax            # 8-byte Spill
	mov	qword ptr [rax], 1
	mov	qword ptr [rax + 8], 0
	mov	qword ptr [rax + 16], 1
	mov	edi, 32
	call	malloc@PLT
	mov	rdx, qword ptr [rsp + 144]      # 8-byte Reload
	mov	rcx, qword ptr [rsp]            # 8-byte Reload
	mov	qword ptr [rsp + 8], rax        # 8-byte Spill
	mov	qword ptr [rax + 8], 1
	mov	qword ptr [rax + 16], rdx
	mov	qword ptr [rax + 24], rcx
	mov	qword ptr [rax], 2
	mov	edi, 32
	call	malloc@PLT
	mov	rdx, qword ptr [rsp + 152]      # 8-byte Reload
	mov	rcx, rax
	mov	rax, qword ptr [rsp + 8]        # 8-byte Reload
	mov	qword ptr [rcx], 1
	mov	qword ptr [rcx + 8], 3
	mov	qword ptr [rcx + 16], rax
	mov	qword ptr [rcx + 24], rdx
	mov	qword ptr [rsp + 200], rcx      # 8-byte Spill
	mov	qword ptr [rsp + 208], rax      # 8-byte Spill
	jmp	.LBB2_1
.Lfunc_end2:
	.size	Upto.upTo, .Lfunc_end2-Upto.upTo
                                        # -- End function
	.globl	Upto.sum                        # -- Begin function Upto.sum
	.p2align	4, 0x90
	.type	Upto.sum,@function
Upto.sum:                               # @Upto.sum
# %bb.0:
	sub	rsp, 280
	mov	rax, rdi
	mov	qword ptr [rsp + 136], rax      # 8-byte Spill
	mov	qword ptr [rsp + 144], rdi      # 8-byte Spill
	mov	rax, rsi
	mov	qword ptr [rsp + 152], rax      # 8-byte Spill
	mov	rax, rsi
	add	rax, 8
	mov	qword ptr [rsp + 160], rax      # 8-byte Spill
	mov	rcx, qword ptr [rsi + 8]
	mov	qword ptr [rsp + 168], rcx      # 8-byte Spill
	mov	eax, 2
	sub	rcx, 2
                                        # implicit-def: $rcx
	mov	qword ptr [rsp + 176], rax      # 8-byte Spill
                                        # implicit-def: $rax
                                        # implicit-def: $rax
	je	.LBB3_4
	jmp	.LBB3_18
.LBB3_18:
	mov	rax, qword ptr [rsp + 168]      # 8-byte Reload
	sub	rax, 3
	je	.LBB3_1
	jmp	.LBB3_19
.LBB3_19:
	jmp	.LBB3_2
.LBB3_1:                                # %"CUpto.List._\E2\88\B7__20"
	mov	rax, qword ptr [rsp + 152]      # 8-byte Reload
	mov	rcx, qword ptr [rax + 16]
	mov	rax, qword ptr [rax + 24]
	mov	edx, 3
                                        # implicit-def: $rsi
	mov	qword ptr [rsp + 176], rdx      # 8-byte Spill
	mov	qword ptr [rsp + 120], rcx      # 8-byte Spill
	mov	qword ptr [rsp + 128], rax      # 8-byte Spill
	jmp	.LBB3_4
.LBB3_2:                                # %FUpto.upTo_20
	mov	rax, qword ptr [rsp + 152]      # 8-byte Reload
	mov	rsi, qword ptr [rax + 16]
	mov	rdx, qword ptr [rax + 24]
	lea	rdi, [rsp + 248]
	call	Upto.upTo@PLT
	mov	rax, qword ptr [rsp + 272]
	mov	rcx, qword ptr [rsp + 264]
	mov	rsi, qword ptr [rsp + 248]
	mov	rdx, qword ptr [rsp + 256]
	mov	qword ptr [rsp + 112], rsi      # 8-byte Spill
	mov	qword ptr [rsp + 176], rdx      # 8-byte Spill
	mov	qword ptr [rsp + 120], rcx      # 8-byte Spill
	mov	qword ptr [rsp + 128], rax      # 8-byte Spill
	jmp	.LBB3_4
# %bb.3:                                # %default_20
.LBB3_4:                                # %continue_20
	mov	rcx, qword ptr [rsp + 112]      # 8-byte Reload
	mov	rax, qword ptr [rsp + 176]      # 8-byte Reload
	mov	rdx, qword ptr [rsp + 120]      # 8-byte Reload
	mov	rsi, qword ptr [rsp + 128]      # 8-byte Reload
	mov	qword ptr [rsp + 96], rsi       # 8-byte Spill
	mov	qword ptr [rsp + 104], rdx      # 8-byte Spill
	cmp	rax, 2
	jne	.LBB3_9
# %bb.5:                                # %"CUpto.List.[]_21"
	mov	rax, qword ptr [rsp + 152]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 160]      # 8-byte Reload
	mov	qword ptr [rcx], 2
	mov	rax, qword ptr [rax]
	mov	qword ptr [rsp + 88], rax       # 8-byte Spill
	cmp	rax, 1
	jne	.LBB3_7
# %bb.6:                                # %"1_23"
	mov	rdi, qword ptr [rsp + 152]      # 8-byte Reload
	call	free@PLT
                                        # implicit-def: $rax
                                        # implicit-def: $rax
	xor	eax, eax
	mov	ecx, eax
	mov	rax, rcx
	mov	qword ptr [rsp + 72], rcx       # 8-byte Spill
	mov	qword ptr [rsp + 80], rax       # 8-byte Spill
	jmp	.LBB3_8
.LBB3_7:                                # %default_23
	mov	rax, qword ptr [rsp + 152]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 88]       # 8-byte Reload
	dec	rcx
	mov	qword ptr [rax], rcx
                                        # implicit-def: $rax
                                        # implicit-def: $rax
	xor	eax, eax
	mov	ecx, eax
	mov	rax, rcx
	mov	qword ptr [rsp + 72], rcx       # 8-byte Spill
	mov	qword ptr [rsp + 80], rax       # 8-byte Spill
	jmp	.LBB3_8
.LBB3_8:                                # %common.ret
	mov	rax, qword ptr [rsp + 136]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 144]      # 8-byte Reload
	mov	rdi, qword ptr [rsp + 72]       # 8-byte Reload
	mov	rsi, qword ptr [rsp + 80]       # 8-byte Reload
                                        # implicit-def: $rdx
                                        # implicit-def: $r8
	mov	qword ptr [rcx], r8
	mov	qword ptr [rcx + 8], rdi
	mov	qword ptr [rcx + 16], rsi
	mov	qword ptr [rcx + 24], rdx
	add	rsp, 280
	ret
.LBB3_9:                                # %"CUpto.List._\E2\88\B7__21"
	mov	rax, qword ptr [rsp + 152]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 96]       # 8-byte Reload
	mov	rdx, qword ptr [rsp + 104]      # 8-byte Reload
	mov	rsi, qword ptr [rsp + 160]      # 8-byte Reload
	mov	qword ptr [rsi], 3
	mov	qword ptr [rax + 16], rdx
	mov	qword ptr [rax + 24], rcx
	mov	rax, qword ptr [rax]
	mov	qword ptr [rsp + 64], rax       # 8-byte Spill
	cmp	rax, 1
	jne	.LBB3_11
# %bb.10:                               # %"1_25"
	mov	rdi, qword ptr [rsp + 152]      # 8-byte Reload
	call	free@PLT
	mov	rax, qword ptr [rsp + 104]      # 8-byte Reload
	mov	qword ptr [rsp + 56], rax       # 8-byte Spill
	jmp	.LBB3_12
.LBB3_11:                               # %default_25
	mov	rax, qword ptr [rsp + 104]      # 8-byte Reload
	mov	rcx, qword ptr [rsp + 152]      # 8-byte Reload
	mov	rdx, qword ptr [rsp + 64]       # 8-byte Reload
	mov	rsi, qword ptr [rsp + 96]       # 8-byte Reload
	mov	rdi, qword ptr [rsi]
	add	rdi, 1
	mov	qword ptr [rsi], rdi
	mov	rsi, qword ptr [rax]
	add	rsi, 1
	mov	qword ptr [rax], rsi
	add	rdx, -1
	mov	qword ptr [rcx], rdx
	mov	qword ptr [rsp + 56], rax       # 8-byte Spill
.LBB3_12:                               # %continue_0.i
	mov	rsi, qword ptr [rsp + 96]       # 8-byte Reload
	mov	rax, qword ptr [rsp + 56]       # 8-byte Reload
	mov	qword ptr [rsp + 16], rax       # 8-byte Spill
	lea	rdi, [rsp + 216]
	call	Upto.sum@PLT
	mov	rax, qword ptr [rsp + 16]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 240]
	mov	rcx, qword ptr [rsp + 232]
	mov	qword ptr [rsp + 24], rcx       # 8-byte Spill
	mov	rcx, qword ptr [rsp + 216]
	mov	rcx, qword ptr [rsp + 224]
	mov	rcx, rax
	add	rcx, 8
	mov	qword ptr [rsp + 32], rcx       # 8-byte Spill
	mov	rcx, qword ptr [rax + 8]
	mov	rax, qword ptr [rax + 16]
	mov	qword ptr [rsp + 40], rax       # 8-byte Spill
	cmp	rcx, 0
	mov	qword ptr [rsp + 48], rax       # 8-byte Spill
	je	.LBB3_14
# %bb.13:                               # %FAgda.Builtin.Nat._-__2.i
	mov	rsi, qword ptr [rsp + 40]       # 8-byte Reload
	mov	rax, qword ptr [rsp + 16]       # 8-byte Reload
	mov	rdx, qword ptr [rax + 24]
	lea	rdi, [rsp + 184]
	call	"Agda.Builtin.Nat._-_"@PLT
	mov	rax, qword ptr [rsp + 208]
	mov	rax, qword ptr [rsp + 200]
	mov	rcx, qword ptr [rsp + 184]
	mov	rcx, qword ptr [rsp + 192]
	mov	qword ptr [rsp + 48], rax       # 8-byte Spill
.LBB3_14:                               # %continue_2.i
	mov	rax, qword ptr [rsp + 16]       # 8-byte Reload
	mov	rdx, qword ptr [rsp + 32]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 48]       # 8-byte Reload
	mov	qword ptr [rsp], rcx            # 8-byte Spill
	mov	qword ptr [rdx], 0
	mov	qword ptr [rax + 16], rcx
	mov	rax, qword ptr [rax]
	mov	qword ptr [rsp + 8], rax        # 8-byte Spill
	cmp	rax, 1
	jne	.LBB3_16
# %bb.15:                               # %"1_4.i"
	mov	rdi, qword ptr [rsp + 16]       # 8-byte Reload
	call	free@PLT
	jmp	.LBB3_17
.LBB3_16:                               # %default_4.i
	mov	rax, qword ptr [rsp + 16]       # 8-byte Reload
	mov	rcx, qword ptr [rsp + 8]        # 8-byte Reload
	add	rcx, -1
	mov	qword ptr [rax], rcx
.LBB3_17:                               # %"Agda.Builtin.Nat._+_.exit"
	mov	rcx, qword ptr [rsp + 24]       # 8-byte Reload
	mov	rax, qword ptr [rsp]            # 8-byte Reload
	add	rax, rcx
                                        # implicit-def: $rcx
                                        # implicit-def: $rcx
	xor	ecx, ecx
                                        # kill: def $rcx killed $ecx
	mov	qword ptr [rsp + 72], rcx       # 8-byte Spill
	mov	qword ptr [rsp + 80], rax       # 8-byte Spill
	jmp	.LBB3_8
.Lfunc_end3:
	.size	Upto.sum, .Lfunc_end3-Upto.sum
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	sub	rsp, 56
	.cfi_def_cfa_offset 64
	mov	edi, 32
	mov	qword ptr [rsp], rdi            # 8-byte Spill
	call	malloc@PLT
	mov	rdi, qword ptr [rsp]            # 8-byte Reload
	mov	qword ptr [rsp + 8], rax        # 8-byte Spill
	mov	qword ptr [rax], 1
	mov	qword ptr [rax + 8], 2
	call	malloc@PLT
	mov	rdi, qword ptr [rsp]            # 8-byte Reload
	mov	qword ptr [rsp + 16], rax       # 8-byte Spill
	mov	qword ptr [rax], 1
	mov	qword ptr [rax + 8], 0
	mov	qword ptr [rax + 16], 50000
	call	malloc@PLT
	mov	rcx, qword ptr [rsp + 8]        # 8-byte Reload
	mov	rsi, rax
	mov	rax, qword ptr [rsp + 16]       # 8-byte Reload
	mov	qword ptr [rsi], 1
	mov	qword ptr [rsi + 8], 4
	mov	qword ptr [rsi + 16], rcx
	mov	qword ptr [rsi + 24], rax
	lea	rdi, [rsp + 24]
	call	Upto.sum@PLT
	mov	rsi, qword ptr [rsp + 40]
	lea	rdi, [rip + ".L%d"]
	add	rsp, 56
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
	sub	rsp, 24
	mov	qword ptr [rsp + 8], rdi        # 8-byte Spill
	mov	rax, qword ptr [rdi]
	mov	qword ptr [rsp + 16], rax       # 8-byte Spill
	cmp	rax, 1
	jne	.LBB5_10
# %bb.1:                                # %"1_26"
	mov	rax, qword ptr [rsp + 8]        # 8-byte Reload
	mov	rax, qword ptr [rax + 8]
	mov	qword ptr [rsp], rax            # 8-byte Spill
# %bb.11:                               # %"1_26"
	mov	rax, qword ptr [rsp]            # 8-byte Reload
	lea	rcx, [rip + .LJTI5_0]
	movsxd	rax, dword ptr [rcx + 4*rax]
	add	rax, rcx
	jmp	rax
.LBB5_2:                                # %common.ret
	add	rsp, 24
	ret
.LBB5_3:                                # %"CUpto.List.[]_27"
	mov	rdi, qword ptr [rsp + 8]        # 8-byte Reload
	call	free@PLT
	jmp	.LBB5_2
.LBB5_4:                                # %"CUpto.List._\E2\88\B7__27"
	mov	rax, qword ptr [rsp + 8]        # 8-byte Reload
	mov	rdi, qword ptr [rax + 16]
	call	drop@PLT
	mov	rax, qword ptr [rsp + 8]        # 8-byte Reload
	mov	rdi, qword ptr [rax + 24]
	call	drop@PLT
	mov	rdi, qword ptr [rsp + 8]        # 8-byte Reload
	call	free@PLT
	jmp	.LBB5_2
.LBB5_5:                                # %Cnat_27
	mov	rdi, qword ptr [rsp + 8]        # 8-byte Reload
	call	free@PLT
	jmp	.LBB5_2
.LBB5_6:                                # %FAgda.Builtin.Nat._-__27
	mov	rax, qword ptr [rsp + 8]        # 8-byte Reload
	mov	rdi, qword ptr [rax + 16]
	call	drop@PLT
	mov	rax, qword ptr [rsp + 8]        # 8-byte Reload
	mov	rdi, qword ptr [rax + 24]
	call	drop@PLT
	mov	rdi, qword ptr [rsp + 8]        # 8-byte Reload
	call	free@PLT
	jmp	.LBB5_2
.LBB5_7:                                # %FUpto.sum_27
	mov	rax, qword ptr [rsp + 8]        # 8-byte Reload
	mov	rdi, qword ptr [rax + 16]
	call	drop@PLT
	mov	rdi, qword ptr [rsp + 8]        # 8-byte Reload
	call	free@PLT
	jmp	.LBB5_2
.LBB5_8:                                # %FUpto.upTo_27
	mov	rax, qword ptr [rsp + 8]        # 8-byte Reload
	mov	rdi, qword ptr [rax + 16]
	call	drop@PLT
	mov	rax, qword ptr [rsp + 8]        # 8-byte Reload
	mov	rdi, qword ptr [rax + 24]
	call	drop@PLT
	mov	rdi, qword ptr [rsp + 8]        # 8-byte Reload
	call	free@PLT
	jmp	.LBB5_2
# %bb.9:                                # %default_27
.LBB5_10:                               # %default_26
	mov	rax, qword ptr [rsp + 8]        # 8-byte Reload
	mov	rcx, qword ptr [rsp + 16]       # 8-byte Reload
	add	rcx, -1
	mov	qword ptr [rax], rcx
	jmp	.LBB5_2
.Lfunc_end5:
	.size	drop, .Lfunc_end5-drop
	.section	.rodata,"a",@progbits
	.p2align	2, 0x0
.LJTI5_0:
	.long	.LBB5_5-.LJTI5_0
	.long	.LBB5_6-.LJTI5_0
	.long	.LBB5_3-.LJTI5_0
	.long	.LBB5_4-.LJTI5_0
	.long	.LBB5_8-.LJTI5_0
	.long	.LBB5_7-.LJTI5_0
                                        # -- End function
	.text
	.globl	dup                             # -- Begin function dup
	.p2align	4, 0x90
	.type	dup,@function
dup:                                    # @dup
# %bb.0:
	mov	rax, qword ptr [rdi]
	add	rax, 1
	mov	qword ptr [rdi], rax
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
