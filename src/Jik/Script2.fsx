#load "Base.fs"
#load "Util.fs"

open System
open Base
open System.IO

type Expr =
    | Int of int
    | Ref of string
    | If of Expr * Expr * Expr
    | Assign of string * Expr
    | Lambda of string list * Expr list
    | Begin of Expr list
    | App of Expr * Expr list
    | Callcc of Expr

let rec findFreeVarsExpr bound = function
    | If (cond, thenc, elsec) ->
        Set.unionMany [
            findFreeVarsExpr bound cond
            findFreeVarsExpr bound thenc
            findFreeVarsExpr bound elsec
        ]
    | Assign (v, rhs) ->
        Set.add v bound
        |> Set.union (findFreeVarsExpr bound rhs) 
    | App (head, tail) ->
        freeVarsExprList bound tail
        |> Set.union (findFreeVarsExpr bound head) 
    | Lambda (vars, body) ->
        let bound' = (Set.union bound (Set.ofList vars))
        freeVarsExprList bound' body
    | Begin exprs ->
        freeVarsExprList bound exprs
    | Callcc expr -> findFreeVarsExpr bound expr
    | Ref name -> 
        if Set.contains name bound then
            Set.empty
        else 
            Set.singleton name
    | _ -> Set.empty

and freeVarsExprList bound exprList =
    let results = List.map (findFreeVarsExpr bound) exprList
    printfn "results: %A expr: %A" results exprList
    Set.unionMany results

let rec findModifiedVars vars = function 
    | Ref name -> 
        Set.empty
    | If (cond, thenc, elsec) ->
        Set.unionMany [
            findModifiedVars vars cond
            findModifiedVars vars thenc
            findModifiedVars vars elsec
        ]
    | Assign (v, rhs) ->
        if Set.contains v vars then Set.singleton v else Set.empty 
        |> Set.union (findModifiedVars vars rhs) 
    | App (head, tail) ->
        findModifiedVarsList vars tail
        |> Set.union (findModifiedVars vars head) 
    | Lambda (args, call) ->
        let vars' = (Set.difference vars (Set.ofList args))
        freeVarsExprList vars' call
    | Begin exprs ->
        freeVarsExprList vars exprs
    | Callcc expr ->
        findModifiedVars vars expr
    | _ -> Set.empty

and findModifiedVarsList vars exprList =
    List.map (findModifiedVars vars) exprList
    |> Set.unionMany

let rec sexprToExpr = function
    | SExpr.Number n ->
        Int n
    | SExpr.Symbol name ->
        Ref name
    | List [SExpr.Symbol "if"; cond; conseq; altern] ->
        If (sexprToExpr cond,
            sexprToExpr conseq,
            sexprToExpr altern)
    | List (Symbol "begin" :: e :: exprs) ->
        let es = List.map sexprToExpr exprs
        Begin (sexprToExpr e :: es)
    | List [Symbol "set!"; Symbol name; rhs] ->
        Assign (name, sexprToExpr rhs)
    | List (Symbol "lambda" :: List args :: body) ->
        Lambda (symbolsToStrings args, List.map sexprToExpr body)
    | List [Symbol "quote"; form] ->
        failwith "" //
    | List (head :: tail) ->
        App (sexprToExpr head, List.map sexprToExpr tail)
    | List [Symbol "call/cc"; sexpr] ->
        Callcc (sexprToExpr sexpr)
    | e -> failwith <| sexprToString e

/// Compile
type Instruction =
    | ReferLocal of int * Instruction
    | ReferFree of int * Instruction
    | AssignLocal of int * Instruction
    | AssignFree of int * Instruction
    | Constant of int * Instruction
    | Return of int
    | Close of int * Instruction * Instruction
    | Test of Instruction * Instruction
    | Apply
    | Conti of Instruction
    | Frame of Instruction * Instruction
    | Argument of Instruction
    | Indirect of Instruction
    | Shift of int * int * Instruction
    | Halt
    | Box of int * Instruction
    | Nuate of int * string

and Value =
    | IntVal of int
    | Closure of Instruction * (Value [])
    | Boxed of Value ref
    | Undefined

let isTail = function
    | Return _ -> true
    | _ -> false

let compileLookup name (locals, free) returnLocal returnFree =
    match Seq.tryFindIndex ((=) name) locals with
    | Some i -> returnLocal i
    | None -> 
        match Seq.tryFindIndex ((=) name) free with
        | Some i -> returnFree i
        | None -> failwithf "compileLookup: name '%s' not found" name

let compileRefer name env next = 
    compileLookup name env 
        (fun i -> ReferLocal (i, next))
        (fun i -> ReferFree (i, next))

let collectFree vars env next =
    let fold next name =
        compileRefer name env next
    Seq.fold fold next vars 

let makeBoxes sets vars next =
    let folder var (i, next) =
        let i' = i - 1
        if Seq.contains var sets
        then i', (Box (i, next))
        else i', next
    let _, result = List.foldBack folder vars (List.length vars, next)
    result

let rec compile expr env s next =
    match expr with
    | Ref name -> 
        let next' =
            if Set.contains name s 
            then Indirect next
            else next
        compileRefer name env next'
    | Int n -> Constant (n, next)
    | Lambda (vars, body) ->
        compileLambda vars body env s next
    | If (cond, thenc, elsec) ->
        compileIf cond thenc elsec env s next
    | Callcc expr ->
        compileCallcc expr env s next
    | App (func, argExprs) ->
        compileApp func argExprs env s next
    | Assign (name, rhs) -> 
        compileAssign name rhs env s next
    | Begin exprs ->
        compileBegin exprs env s next

and compileLambda vars body env s next =
    let free = freeVarsExprList (Set.ofList vars) body
    printfn "free: %A" free
    let sets = findModifiedVarsList (Set.ofList vars) body 
    let s' = Set.union sets (Set.intersect s free)
    let env' =  (vars, free) 
    let next' = (Return (List.length vars))    
    let compiledBody = compile (Begin body) env' s' next'
    let boxed = makeBoxes sets vars compiledBody
    collectFree free env <| Close (Seq.length free, boxed, next)

and compileIf cond thenc elsec env s next =
    let compiledThen = compile thenc env s next
    let compiledElse = compile elsec env s next
    compile cond env s <| Test (compiledThen, compiledElse)

and compileCallcc expr env s next =
    let next' =
        match next with
        | Return n -> Shift (1, n, Apply)
        | _ -> Apply
    let compiledSub = compile expr env s next'
    let conti = Conti (Argument compiledSub)
    if isTail next
    then conti
    else Frame (next, conti)

and compileApp func argExprs env s next =
    let funcNext =
        match next with
        | Return n -> Shift (List.length argExprs, n, Apply)
        | _ -> Apply
    let compiledFunc = compile func env s funcNext
    let fold next arg =
        compile arg env s (Argument next)
    let compiledArgs = List.fold fold compiledFunc argExprs
    if isTail next
    then compiledArgs
    else Frame (next, compiledArgs)

and compileAssign name rhs env s next =
    compileLookup name env
        (fun i -> compile rhs env s (AssignLocal (i, next)))
        (fun i -> compile rhs env s (AssignFree (i, next)))

and compileBegin exprs env s next =
    let fold expr next = 
        compile expr env s next
    List.foldBack fold exprs next

let globals = [
    "fact"
    "*"
    "-"
    "<="
] 

/// VM
let stack = Array.create 1000 Undefined

let push expr sp =
    Array.set stack sp expr
    sp + 1

let stackGet sp i =
    Array.get stack (sp - i - 1)

let stackSet sp i v =
    Array.set stack (sp - i - 1) v

let shiftArgs n m s =
    failwith ""

let closure body n sp =
    let v = Array.create n Undefined
    let rec loop i =
        if i <= n then
            Array.set v i (stackGet sp i)
    loop 0
    Closure (body, v)

let closureBody = fst

let closureIndex (_, v) i =
    Array.get v i

let unbox = function
    | Boxed v -> !v
    | _ -> failwith "unbox: not a box"

let box v = Boxed (ref v)

let boolify = function
    | IntVal 0 -> false
    | _ -> true

let assignRef v = function
    | Boxed r ->
        r := v
    | _ -> failwith "assignRef: "

let continuation sp =
    closure (Nuate (sp, "v")) 1 sp

let rec VM (accum : Value) expr frame clos sp =
    match expr with
    | Halt -> accum
    | ReferLocal (i, next) ->
        VM (stackGet frame i) next frame clos sp
    | ReferFree (i, next) ->
        VM (closureIndex clos i) next frame clos sp
    | Indirect next ->
        VM (unbox accum) next frame clos sp
    | Constant (n, next) ->
        VM (IntVal n) next frame clos sp
    | Close (n, body, next) ->
        VM (closure body n sp) next frame clos sp
    | Box (n, next) ->
        stackSet sp n (box (stackGet sp n))
        VM accum next frame clos sp
    | Test (theni, elsei) ->
        let next =
            if boolify accum
            then theni
            else elsei
        VM accum next frame clos sp
    | AssignLocal (i, next) ->
        assignRef (stackGet frame i) accum
        VM accum next frame clos sp
    | AssignFree (i, next) ->
        assignRef (closureIndex clos i) accum
        VM accum next frame clos sp
    | Conti next ->
        VM (continuation sp) next frame clos sp
    | Nuate _ ->
        failwith "not implemented"
    | Frame (ret, next) ->
        VM accum next frame clos (push ret (push frame (push clos sp)))

let compileString s =
    let e = stringToSExpr s |> sexprToExpr
    compile e ([], Set.ofList globals) Set.empty Halt

let e = "(lambda (x) (begin
    (set! x 2)
    (f x (lambda (y) (+ y x)))))"
let e2 = "(begin
(set! fact (lambda (n)
    (if (<= n 1)
        n
        (* n  (fact (- n 1))))))
(fact 50))"
let e3 = "(lambda (n)
    (if (<= n 1)
        n
        (* n (fact (- n 1)))))"
compileString e2