#load "references.fsx"

type Ty = Mi of int * int * int

let i = Mi (1,2,3)

match 1, i with
| n, Mi _ -> printfn "89324982374723984 %d " n