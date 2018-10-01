module Cps

open Base

type Expr =
    | Int of int
    | Bool of bool
    | Ref of string
    | Lambda of string list * Call

and Call =
    | If of Expr * Call * Call
    | Assign of string * Expr * Call
    | App of Expr * Expr list

(*

(if (if 1 2 3) 4 5)
(lambda (v) (if v 4 5)) 



*)

let rec freeVarsCall bound = function
    | If (cond, thenc, elsec) ->
        Set.union (freeVarsExpr bound cond) 
            (Set.union (freeVarsCall bound thenc) 
                (freeVarsCall bound elsec))
    | Assign (v, rhs, next) ->
        Set.union (Set.add v bound) <| 
            Set.union (freeVarsExpr bound rhs) 
                (freeVarsCall bound next)
    | App (head, tail) ->
        Set.union (freeVarsExpr bound head)
            (List.map (freeVarsExpr bound) tail |> List.fold Set.union Set.empty)
and freeVarsExpr bound = function
    | Lambda (vars, call) ->
        let bound' = (Set.union bound (Set.ofList vars))
        freeVarsCall bound' call
    | Ref name -> Set.add  name bound
    | _ -> bound

    

let emitSchemeEntry out env si body = 
    fprintf out "  .text"
    fprintf out "  .globl schemeEntry"
    fprintf out "  .def schemeEntry; .scl 2; .type 32; .endef"
    fprintf out "  .seh_proc schemeEntry"
    fprintf out "schemeEntry:"
    fprintf out "  pushq %%rbp"
    fprintf out "  .seh_pushreg %%rbp"
    fprintf out "  movq %%rsp, %%rbp"
    fprintf out "  .seh_setframe %%rbp, 0"
    fprintf out "  .seh_endprologue"
    fprintf out "  push %%rbp"
    fprintf out "  push %%rbx"
    fprintf out "  push %%rsi"
    fprintf out "  push %%rdi"
    fprintf out "  push %%r15"
    fprintf out "  mov %%rsp, %%r15"
    fprintf out "  mov %%rcx, %%rsp"   
    fprintf out "  mov %%rdx, %%rbp"
    
    fprintf out "  mov %%r15, %%rsp"
    fprintf out "  pop %%r15"
    fprintf out "  pop %%rdi"
    fprintf out "  pop %%rsi"
    fprintf out "  pop %%rbx"
    fprintf out "  popq %%rbp"
    fprintf out "  popq %%rbp"
    fprintf out "  ret"
    fprintf out "  .seh_endproc"