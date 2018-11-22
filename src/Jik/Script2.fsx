open System.IO

#load "references.fsx"

open Core

let e2 = "(define (one) 1)
(+ (one) 1)"
Core.stringToProgram e2
|> Core.convertGlobalRefs