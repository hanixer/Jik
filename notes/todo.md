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

- [ ] Change alignment of stack pointer from 32 to 16.

Need to save all args on the root stack, so we need another loop?
First, put all arguments on the root stack.

Necessary to memset root stack before use.