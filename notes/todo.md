1. Basic IO.
	- Add support in code gen.
	- Add support in Core language - add separate case for foreign call.
	- Before emitting name - add prefix to foreign function name - like "s_".
2. Separate compilation of user code and standard library.
	- What code will go into library?
	- Which primitives will be implemented directly, and which will go into library?
	- Each file would have entry point.
	- During linking all entry points will be collected and
	  executed in such an order, so that library code is initialized before
	  other code.
	- How can we distinguish between global variable and free variable?
2. Safe primitives.
	- Run time checks should be placed in primitive operation invocations.
	  If type tag is incorrect - error should be thrown.
	- First, arity must be checked in compile time.
	  In LiSP book, there is a global description table,
	  which contains information about arity of each primitive.
	  This table is used during compilation.
3. Implement variable arity functions.
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