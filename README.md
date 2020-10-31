# Jik
A compiler from a subset of Scheme language to x86-64.

# Supported features
- I/O console
- Buffered file I/O
- Pairs
- Symbols
- Strings
- Vectors
- Garbage collection
- Complex constants
- `do` form
- `define` form
- `let*` form
- `apply` form
- Floating point numbers

# Building
To build it you need to instal .NET Core 3.0.

Also you need to install gcc with cygwin. gcc needs to be on PATH. It is used to create executables and linking with runtime library.

It works on Windows, but I haven't tested it on other systems. It should probably work on Linux if you install gcc and .NET Core.

# Running
Example of running the compiler:
```
dotnet run -p src\CompilerDriver examples\queens.scm
```

In the result `misc\a.exe` file must be produced.

To run the test suite execute:
```
dotnet run -p src\App
```