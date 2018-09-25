	.file	"g.c"
	.text
	.globl	schemeEntry
	.def	schemeEntry;	.scl	2;	.type	32;	.endef
	.seh_proc	schemeEntry
schemeEntry:
	pushq	%rbp
	.seh_pushreg	%rbp
	movq	%rsp, %rbp
	.seh_setframe	%rbp, 0
	.seh_endprologue
	movl	$9821, %eax
	popq	%rbp
	ret
	.seh_endproc
	.ident	"GCC: (GNU) 7.3.0"
