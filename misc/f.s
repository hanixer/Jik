	.text
	.def	 main;
	.scl	2;
	.type	32;
	.endef
	.globl	main
	.p2align	4, 0x90
main:                                   # @main
.Ltmp0:
.seh_proc main
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.seh_stackalloc 8
.Ltmp2:
	.seh_endprologue
	movl	$123786321, %eax        # imm = 0x760D451
	movl	$0, 4(%rsp)
	popq	%rcx
	retq
	.seh_handlerdata
	.text
.Ltmp3:
	.seh_endproc


