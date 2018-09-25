	.file	"runner.c"
	.text
	.section .rdata,"dr"
.LC0:
	.ascii "%d\12\0"
.LC1:
	.ascii "#f\0"
.LC2:
	.ascii "#t\0"
.LC3:
	.ascii "()\0"
.LC4:
	.ascii "#\\%s\12\0"
.LC5:
	.ascii "<unknown 0x%08x>\0"
	.text
	.def	printPtr;	.scl	3;	.type	32;	.endef
	.seh_proc	printPtr
printPtr:
	pushq	%rbp
	.seh_pushreg	%rbp
	movq	%rsp, %rbp
	.seh_setframe	%rbp, 0
	subq	$48, %rsp
	.seh_stackalloc	48
	.seh_endprologue
	movl	%ecx, 16(%rbp)
	movl	16(%rbp), %eax
	andl	$3, %eax
	testl	%eax, %eax
	jne	.L2
	movl	16(%rbp), %eax
	movl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	movl	%eax, %edx
	leaq	.LC0(%rip), %rcx
	call	printf
	jmp	.L8
.L2:
	cmpl	$47, 16(%rbp)
	jne	.L4
	leaq	.LC1(%rip), %rcx
	call	puts
	jmp	.L8
.L4:
	cmpl	$111, 16(%rbp)
	jne	.L5
	leaq	.LC2(%rip), %rcx
	call	puts
	jmp	.L8
.L5:
	cmpl	$63, 16(%rbp)
	jne	.L6
	leaq	.LC3(%rip), %rcx
	call	puts
	jmp	.L8
.L6:
	movl	16(%rbp), %eax
	movzbl	%al, %eax
	cmpl	$15, %eax
	jne	.L7
	movb	$0, -6(%rbp)
	movb	$0, -5(%rbp)
	movl	16(%rbp), %eax
	shrl	$8, %eax
	movb	%al, -6(%rbp)
	leaq	-6(%rbp), %rax
	movq	%rax, %rdx
	leaq	.LC4(%rip), %rcx
	call	printf
	jmp	.L8
.L7:
	movl	16(%rbp), %edx
	leaq	.LC5(%rip), %rcx
	call	printf
.L8:
	nop
	addq	$48, %rsp
	popq	%rbp
	ret
	.seh_endproc
	.def	__main;	.scl	2;	.type	32;	.endef
	.globl	main
	.def	main;	.scl	2;	.type	32;	.endef
	.seh_proc	main
main:
	pushq	%rbp
	.seh_pushreg	%rbp
	movq	%rsp, %rbp
	.seh_setframe	%rbp, 0
	subq	$32, %rsp
	.seh_stackalloc	32
	.seh_endprologue
	call	__main
	call	schemeEntry
	movl	%eax, %ecx
	call	printPtr
	movl	$0, %eax
	addq	$32, %rsp
	popq	%rbp
	ret
	.seh_endproc
	.ident	"GCC: (GNU) 7.3.0"
	.def	printf;	.scl	2;	.type	32;	.endef
	.def	puts;	.scl	2;	.type	32;	.endef
	.def	schemeEntry;	.scl	2;	.type	32;	.endef
