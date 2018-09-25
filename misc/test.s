  .text
  .globl schemeEntry
  .def schemeEntry; .scl 2; .type 32; .endef
  .seh_proc schemeEntry
schemeEntry:
  pushq %rbp
  .seh_pushreg %rbp
  movq %rsp, %rbp
  .seh_setframe %rbp, 0
  .seh_endprologue
  push %rbp
  push %rbx
  push %rsi
  push %rdi
  push %r15
  mov %rsp, %r15
  mov %rcx, %rsp
  mov %rdx, %rbp
  mov $8, %rax
  mov %rax, 0(%rbp)
  mov %rax, %rcx
  mov %rax, %rcx
  mov %rbp, %rax
  or $5, %rax
  add %rcx, %rbp
  add $8, %rbp
  mov %rax, -8(%rsp)
  mov -8(%rsp), %rax 
  mov %rax, %rcx
  mov $0, %rax
  mov %rax, %rdx
  mov $111, %rax
 mov %rax, -5(%rcx, %rdx, 8)
  mov -8(%rsp), %rax 
  mov %rax, %rcx
  mov $4, %rax
  mov %rax, %rdx
  mov $47, %rax
 mov %rax, -5(%rcx, %rdx, 8)
  mov -8(%rsp), %rax 
  mov %r15, %rsp
  pop %r15
  pop %rdi
  pop %rsi
  pop %rbx
  popq %rbp
  popq %rbp
  ret
  .seh_endproc
