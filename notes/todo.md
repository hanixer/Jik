1. Basic IO.
2. Safe primitives.
	- Run time checks should be placed in primitive operation invocations.
	  If type tag is incorrect - error should be thrown.
	- First, arity must be checked in compile time.
	  In LiSP book, there is a global description table,
	  which contains information about arity of each primitive.
	  This table is used during compilation.