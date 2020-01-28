- [x] Complex constants.
- [x] Output ports.
- [ ] Safe primitives.
- [x] Symbols.
- [x] Change initialization of strings.
- [x] Report error on heap overflow.
- [x] Rework desugar process.
- [x] Add internal definitions.
- [x] Throw an error if a global variable is not initialized.
- [ ] 'do' form
- [ ] cadr caddr
- [ ] let*
- [ ] Find a few benchmarks, that could be used for comparison of safe primitives.
- [ ] floating point number, then we can do ray tracer.
- [ ] garbage collection - far far away...

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

# Output ports
Unique identifier - which to choose?

Structure:
0. id
1. string filename
2. int file desc
3. string output buf
4. int next pos
5. int buf size

# Issue with library initialization
We have a problem:
String symbol uses string->symbol function for its creation, but function
is not yet initialized.
How to initialize library before anything else?

# Separate compilation
So the most problematic issues with separate compilation are:
- During compilation of user code, how to provide external symbols defined outside user code?
	- We can define a list of defined symbols and update it each time we add a primitive.
	- This scheme does not work when we will compile more than one user file.
- How to get an entry point of an compilation unit?
	- An entry point can have the same name as file.

Each file corrsponds to compilation unit.
This CU should have names, that it define.

compile lib file 1 -> libf1.s
compile lib file 2 -> libf2.s
...
compile main file -> main.s

build: gcc runtime.c libf1.s libf2.s ... main.s -> a.exe

another approach:
build all modules the same way.
but then generate a file, that will initialize all modules and will contain schemeEntry.

compile : source name isMain -> file

# Stack alignment and C function()
Before call Stack must be aligned on 16 byte boundary.

# Initialization of strings
First, strings should be collected and substited by their names.
Add a field for string constants to intermediate and Codegen for.
During code generation make labels for each string:
```
	.section .rdata,"dr"
	.align 8
.LC0:
	.quad (length or stringTag)
	.ascii "When are you planing to start?\0"

```

# Report error on heap overflow
Add heap top pointer, and compare it with current freePointer.
If freePointer is >= than top - report error and exit.
Which primitives use freePointer?
cons, make-*(vector, string, closure)

# Internal definitions
Change behaviour of global symbols.
Across all modules, there should be only one global variable.
So each module references global variables. These variables are collected during compilation.
Then main module will define all of the global variables, and set all of them to undefined value.
Only after that entry points of modules will be called.
Define forms should be transformed in top level:
```
(define a 1) => (set! a 1)
(define a (lambda (x) x)) => (set! a (lambda ...))
(define (a x) x) => (set! a (lambda ...))
(define (a . x) x) => (set! a (lambda x x))
```
In local scope:
Collect all definitions and bind them to name.
Data defines are collected on top level.
Func defines are combined into letrec.

Make a new pass on Core form, that will change globals references.
This pass will need environment to track.

```
(lambda (x) (lambda (y) x))
```

# Reporting an error if a global var is not initialized
Pass address of a global var to some reginer (e.g. rcx).
Jump to an error handler.
Error handler call C function.
C function get an address or index of global variable.
Then it get string.
And print the string.
String must be original symbol name.

Problem: each module refers to a global variable.
But index of that variable will be available only after all modules are processed.
So how that can be handled?
Create yet another table, which will map from global to index.
We can first collect global variables, create a list of it and pass that mapping to code generation.
Or we can pass to rcx the address of the variable.
And then find an address of the variable in a table.
What will be store in the table?
Pairs:
	First element is an address of global
	Second element is a string.

# Garbage collector
- [x] Expose allocation.
- [ ] Table of globals and constants for GC roots.
- [ ] Save closure pointer before collect.


Copying garbage collector.
There are two spaces for objects.
When current space is filled, the collector reachable objects to the other space.
Spaces swaps their roles.

Which objects are reachable?
Those which are referenced from the stack.
Also objects, that are referenced via closures.
All closures are saved on the stack.
And one closure is stored in rsi register.

Definetely, collector needs to scan the stack to search the roots.

When garbage collector is triggered?
Each time data is allocated, free pointer is checked for available space.
If no space is available, GC is triggered.
Each allocation must know how much space is needed.

How know determine the size?
For pair (cons) the size is 2 words.
For Closure it is one word + a word for each free variable.
For string and vectors it can be determined in runtime.
Make-Vector has number of elements n.
n + 1 is the number to allocate.
In which phase this can be done?
This will be done in core form.
What about closures? they are added in intermediate phase.
Add comparison of two global values.
Then make s foreign call, and pass arguments.

How to make shadow stack aka root stack?
During assignment variables to slots, how to detect that variable
should go to a root slot.
We can collect such variables into a list.
Find all declarations of variables initialized by make vector string closure, and cons.
Store it in R15
rsi (closure pointer) is also strored to shadow stack before call and before garbage collection.

How to represent call collect()?
We can define it as a primitive, that will receive one parameter - allocation size.
During codegen it will be converted to a foreign call. It will pass r15 and allocation size.

Where to put calls to allocate()?
Calls to allocate will be added during instruction selection.

What additional data fields must be added to Program records?

What must be added to each phase?
After closure conversion add phase: revealAllocations, that will put pointer values to upper bound and call collect().
```
(let ([v (make-vector 3)]
      [u (make-vector 1)])
	(vector-set! u 0 2)
	(vector-set! v 0 40)
	(vector-set! v 1 #t)
	(vector-set! v 2 u)
	(if (vector-ref v 1)
		(+ (vector-ref v 0)
		   (vector-ref (vector-ref v 2) 0))
		44))
```
```
def schemeEntry <> ()
schemeEntry ()
n = 3
calculate number of bytes.
n33 = (n + 1) * wordSize
if freePointer + n33 < topHeap then jmp L33 else jmp L22

l22()
collect(rsp)
jmp L33

L33()

v = MakeVector n
```

How garbage collector will know about global variables?
Gather all variables into a table.
That will be an array of pointers.
Change printing of constants - add ".global" to them also.
Retrieve all globals and constants.
Write table "globalRootTable".
garbageCollector will use this table.
