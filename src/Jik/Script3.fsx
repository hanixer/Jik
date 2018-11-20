#load "references.fsx"

open Intermediate

let a = replaceClosureRefs ["a"; "b"] ["a"; "c"; "d"; "b"]