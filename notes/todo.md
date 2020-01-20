- [ ] Complex constants.
- [ ] Output ports.
- [ ] Safe primitives.
- [ ] Symbols.

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

# String constants
Add new primitives: constant ref and constant set.
```
(cons "123" "abc")
=>
((lambda (str1 str2)
	(string-set! str1 0 '1')
	(string-set! str1 1 '2')
	...
	(string-set! str2 0 'a')
	(string-set! str2 1 'b')
	...
	...
	...
	(cons str1 str2)) (make-string 3) (make-string 3))
```

# Complex constants
Now we need to collect all constants.
We walk through the Core program. If constant was already seen, we replace it with reference to global
variable.

How to transfer constants from SExpressions to core?
We could pass the SExpr piece, and then transform it.

String is passed as it is.
We could make  a case for symbols and pass symbols like strings.
What about lists?
'(1 2 3) => (prim cons 1 (prim cons 2 (prim cons 3 '())))

# Symbols
For symbols support we need to add primitive make-symbol.

How to traslate symbols?

We first need to allocate string, that corresponds to it,
and then allocate symbol and bind it to unique location.
Later all references should be to that location.

# Apply
It is neccessary to change 'apply' behaviour.
Currently, only (apply f list) is supported.
We also need to support (apply f x y z list) form.
That is, pass x, y and z as in regular call, but split list argument into components.
Also, tail call form for apply is not supported.
Last arguments must be list, or nil.