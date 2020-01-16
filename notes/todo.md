After variable arity functions, we can implement library functions like (vector 1 2 3) => #(1 2 3).
So we return to separate compilation.

1. Basic IO. Done.
	- Add support in code gen.
	- Add support in Core language - add separate case for foreign call.
	- Before emitting name - add prefix to foreign function name - like "s_".
2. Separate compilation of user code and standard library.
	- What code will go into library?
	- Which primitives will be implemented directly, and which will go into library?
		- fx+ - can be implemented in assembly, (+ 1 2 3 4....) - can go to the library.
	- Each file would have entry point.
	- During linking all entry points will be collected and
	  executed in such an order, so that library code is initialized before
	  other code.
	- How can we distinguish between global variable and free variable?
		- If variable is not defined as an argument (every local var is argument because
		  every let form is transformed to lambda invocation), then this variable is a global.
		  But we need to consider alpha renaming also. If variable is global we should not rename it.
		  Probably we need to duplicate free variable analysis.
		  Are aditional variable generated during Core -> Intermediate translation? - Yes, a lot of them. For example,
		  for lam, block names and arguments. But these variables are not considered during free variable analysis, right?
		  - No, they are considered.

2. Safe primitives.
	- Run time checks should be placed in primitive operation invocations.
	  If type tag is incorrect - error should be thrown.
	- First, arity must be checked in compile time.
	  In LiSP book, there is a global description table,
	  which contains information about arity of each primitive.
	  This table is used during compilation.
3. Implement variable arity functions. Done.
   For that we need to implement error reporting, when function has wrong number of arguments.
   As simple implementation we can just report that error happend.
   Then we can also pass some symbol describing the procedure
   and actual and expected numbers of arguments.
   We will append error handler code to the main function. Label name will be visible to compiled code.
   If error condition happens, then we jump to that label.
   Then call to C function error() happens and we exit with -1 or 1.
   Now we need to change so that call the closure directly without moving it to rax.

   So how should we implement varargs?

   Caller should:
     - Just pass arguments as usual and pass number of arguments in rcx

   Callee should:
     - Check number of arguments. The number of arguments should be >= then number of parameters.
	 - Construct the list from rest arguments.

   How list construction should be performed?

   Take last argument (if it available) cons it with null.
   Take next argument cons it.

# Call convention
Arguments are passed via stack. When function is called first passed argument
is at rsp - 16, second at rsp - 24, etc.

# Separate compilation of user code and standard library
High level build of binary:
1. Take a source file or a string from user.
2. Compile and make assembly file for all library files.
	- They can be assembled together into one .s file.
3. Compile and make assembly file for user code.
4. Send to gcc runtime.c, library *.s, user .s.
5. We have a binary

Optionally, library code can be rebuilt only when needed.
For example when we run tests. Before the run, we build
library, and then just link .s or .o to the test code.

Library will be placed in separate directory like library.

Use cases for the compiler driver:
- One source file
- Source in a string

Test driver could use compiler driver. It will just
run produced executable.

Library will contain the same language that is available for user.
It will define utility functions, such as (vector), (string).

Each library file should have an entry point - a function,
that main function will call. These entry points must be executed
before control will pass to the user code.

```
file library.scm
(define list
	(lambda args args))

file user.scm
(list (list 1 2) 3)
```

```
file library.s
##############
.global
list:
	...
	...
	mov something to %rax

	.global
libraryEntry0:
	initialize something
	...
	retq

file user.s
###########

userEntry:
	mov -1(%edi), %edi
	call *%edi

schemeEntry:
	push
	push
	push
	push
	call libraryEntry0
	call userEntry0
	pop
	pop
	pop
	pop
```
Then these two files and runtime.c assembled into exe.

So I think, for it would not work like chez or like ikarus, which runs interpreter, which dynamically compiles native code.
It will work like gcc: take files and compile it to exe.

For implementing optional compilation of library, we need to think how to find entry points?
We can have some naming conventions for entry points, and just scan through library directory and
collect file names, from these file names we will get entry points. But that details. For beginning, we can do without it and just compile library every time we compile user code.

So we will add a project called CompilerDriver - command line utility, that will use Jik.dll.
Test driver can use functions from CompilerDriver.dll.

For beginning, we will support just one library file and just one user file.

We need to pass to all our functions to say that main scheme entry should not (or should) be generated.
This option should be present per file. And should be added to all Program structures.
Some boolean flag like "IsMainFile", i.e. main file contains main entry point.