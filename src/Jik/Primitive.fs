module Primitive

type Prim =
    | Add
    | Sub
    | Mul
    | Eq
    | Lt
    | Le
    | Gt
    | Ge
    | Not
    | IsFixnum
    | IsBoolean
    | Cons
    | IsPair
    | IsNull
    | Car
    | Cdr
    | SetCar
    | SetCdr
    | MakeVector
    | IsVector
    | VectorLength
    | VectorRef
    | VectorSet
    | MakeString
    | IsString
    | StringLength
    | StringRef
    | StringSet
    | StringInit
    | MakeClosure
    | ClosureRef
    | IsProcedure
    | GlobalRef
    | GlobalSet
    | IsZero
    | NumberToChar
    | CharToNumber
    | IsChar
    | Apply
    | MakeSymbol
    | SymbolString
    | IsSymbol
    | Error // this primitive receives a single argument - a string. Should be called from scheme library.

let stringPrimop = [
    "+", Add
    "-", Sub
    "fx+", Add
    "fx-", Sub
    "*", Mul
    "fx*", Mul
    "eq?", Eq
    "<", Lt
    "<=", Le
    ">", Gt
    ">=", Ge
    "not", Not
    "fixnum?", IsFixnum
    "number?", IsFixnum
    "boolean?", IsBoolean
    "cons", Cons
    "pair?", IsPair
    "null?", IsNull
    "car", Car
    "cdr", Cdr
    "set-car!", SetCar
    "set-cdr!", SetCdr
    "make-vector", MakeVector
    "vector?", IsVector
    "vector-length", VectorLength
    "vector-ref", VectorRef
    "vector-set!", VectorSet
    "make-string", MakeString
    "string?", IsString
    "string-length", StringLength
    "string-ref", StringRef
    "string-set!", StringSet
    "procedure?", IsProcedure
    "zero?", IsZero
    "fxzero?", IsZero
    "number->char", NumberToChar
    "char->number", CharToNumber
    "fixnum->char", NumberToChar
    "char->fixnum", CharToNumber
    "char?", IsChar
    "apply", Apply
    "make-symbol", MakeSymbol
    "symbol-string", SymbolString
    "symbol?", IsSymbol
    // "error", Error
]

let libraryFunctions = [
    "string->symbol"
    "list"
    "empty?"
    "memq"
    "list?"
    "length"
    "string=?"
    "vector"
    "output-port?"
    "open-output-file"
    "close-output-port"
    "flush-output-port"
    "current-output-port"
    "input-port?"
    "open-input-file"
    "close-input-port"
    "current-input-port"
    "write-char"
    "read-char"
    "newline"
    "exit-scheme"
]