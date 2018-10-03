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
        Set.difference (Set.singleton v) bound
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
    Set.unionMany results

let freeVarsInBody vars body =
    let s = freeVarsExprList (Set.ofSeq vars) body
    let result =  Set.toSeq s
    printfn "freeVarsInBody: vars %A body %A result %A" vars body result
    result

let rec findModifiedVars (vars : string seq) expr =
    match expr with
    | Ref name -> 
        Set.empty
    | If (cond, thenc, elsec) ->
        Set.unionMany [
            findModifiedVars vars cond
            findModifiedVars vars thenc
            findModifiedVars vars elsec
        ]
    | Assign (v, rhs) ->
        if Seq.contains v vars then Set.singleton v else Set.empty 
        |> Set.union (findModifiedVars vars rhs) 
    | App (head, tail) ->
        findModifiedVarsList vars tail
        |> Set.union (findModifiedVars vars head) 
    | Lambda (args, body) ->
        let vars' = (Set.difference (Set.ofSeq vars) (Set.ofList args))
        findModifiedVarsList vars' body
    | Begin exprs ->
        findModifiedVarsList vars exprs
    | Callcc expr ->
        findModifiedVars vars expr
    | _ -> Set.empty
    |> (fun x -> printfn "findModifiedVars: vars %A expr %A result %A" vars expr x ; x)

and findModifiedVarsList vars exprList =
    List.map (findModifiedVars vars) exprList
    |> Set.unionMany
    |> (fun x -> printfn "findModifiedVarsList: vars %A exprList %A result %A" vars exprList x ; x)

let findModiedVarsInBody free argVars body =
    let sets = findModifiedVarsList (Set.ofSeq argVars) body 
    Set.union sets (Set.intersect sets (Set.ofSeq free))

let rec sexprToExpr sexpr =
    printfn "sexprToExpr: %s" (sexprToString sexpr)
    match sexpr with
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
        failwith "sexprToExpr: quote is not supported"
    | List (Symbol "let" :: List bindings :: body) ->
        let folder sexpr (names, initExprs) =
            match sexpr with
            | List [Symbol name; initExpr] -> name :: names, initExpr :: initExprs
            | _ -> failwith "sexprToExpr: let: wrong bindings"
        let names, initExprs = List.foldBack folder bindings ([], [])
        let lam = Lambda (names, List.map sexprToExpr body)
        let argExprs = List.map sexprToExpr initExprs
        App (lam, argExprs)
    | List (head :: tail) ->
        App (sexprToExpr head, List.map sexprToExpr tail)
    | List [Symbol "call/cc"; sexpr] ->
        Callcc (sexprToExpr sexpr)
    | e -> failwith <| sexprToString e

/// Compile
type Instruction =
    | ReferLocal of int * Instruction
    | ReferFree of int * Instruction
    | ReferGlobal of int * Instruction
    | ReferPredefined of int * Instruction
    | AssignLocal of int * Instruction
    | AssignFree of int * Instruction
    | AssignGlobal of int * Instruction
    | Constant of int * Instruction
    | Return of int
    | Close of int * Instruction * Instruction
    | Test of Instruction * Instruction
    | Apply
    | PredefinedInstr of int * PredefinedFunc
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
    | PredefinedVal of int * PredefinedFunc
    | Boxed of Value ref
    | Undefined

and PredefinedFunc = (int -> Value)

let mutable globalEnv : string list = []
let mutable predefinedEnv : string list = []

let instructionToString = function
    | ReferLocal (n, _) -> sprintf "ReferLocal %d" n
    | ReferFree(n, _) -> sprintf "ReferFree %d" n
    | AssignLocal(n, _) -> sprintf "AssignLocal %d" n
    | AssignFree(n, _) -> sprintf "AssignFree %d" n
    | Constant(n, _) -> sprintf "Constant %d" n
    | Return(n) -> sprintf "Return %d" n
    | Close(n, _, _) -> sprintf "Close %d" n
    | Test(_, _) -> sprintf "Test"
    | Apply -> sprintf "Apply"
    | Conti(_) -> sprintf "Conti" 
    | Frame(_, _) -> sprintf "Frame" 
    | Argument(_) -> sprintf "Argument" 
    | Indirect(_) -> sprintf "Indirect" 
    | Shift(n, _, _) -> sprintf "Shift %d" n
    | Halt -> sprintf "Halt" 
    | Box(_, _) -> sprintf "Box" 
    | Nuate(_, _) -> sprintf "Nuate"
    | ReferGlobal(n, _) -> sprintf "ReferGlobal %d" n
    | ReferPredefined(n, _) -> sprintf "ReferPredefined %d" n
    | AssignGlobal(n, _) -> sprintf "ReferLocal %d" n
    | PredefinedInstr(_, _) -> failwith "Not Implemented"

let rec valueToString = function
    | IntVal n -> sprintf "%d" n
    | Boxed r -> sprintf "<box %s>" (valueToString !r)
    | Closure _ -> "<closure>"
    | Undefined -> "<undefined>"
    | PredefinedVal(_, _) -> failwith "Not Implemented"

let isTail = function
    | Return _ -> true
    | _ -> false



let compileLookup name (locals, free : string seq) returnLocal returnFree returnGlobal returnPredefined =
    let tryFind = Seq.tryFindIndex ((=) name)
    match tryFind locals with
    | Some i -> returnLocal i
    | None -> 
        match tryFind free with
        | Some i -> returnFree i
        | None -> 
            match tryFind globalEnv with
            | Some i -> returnGlobal i
            | None ->
                match tryFind predefinedEnv with
                | Some i -> returnPredefined i
                | None -> failwithf "compileLookup: name '%s' not found" name

let compileRefer name env next = 
    compileLookup name env 
        (fun i -> ReferLocal (i, next))
        (fun i -> ReferFree (i, next))
        (fun i -> ReferGlobal (i, next))
        (fun i -> ReferPredefined (i, next))

let collectFree vars env next =
    let fold next name =
        compileRefer name env (Argument next)
    let result = Seq.fold fold next vars 
    result

let makeBoxes sets vars next =
    let folder var (i, next) =
        let i' = i - 1
        if Seq.contains var sets
        then i', (Box (i', next))
        else i', next
    let _, result = List.foldBack folder vars (List.length vars, next)
    printfn "makeBoxes: sets %A vars %A result %A " sets vars result
    result

type Env = string list * string seq

let rec compile expr (env : Env) s next =
    match expr with
    | Ref name -> 
        let next' =
            if Set.contains name s 
            then Indirect next
            else next
        compileRefer name env next'
    | Int n -> Constant (n, next)
    | Lambda (vars, body) ->
        compileLambda vars body env next
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

and compileLambda vars body env next =
    let free = freeVarsInBody vars body
    let sets' = findModiedVarsInBody free vars body
    let env' : Env =  (vars, free) 
    let next' = (Return (List.length vars))    
    let compiledBody = compile (Begin body) env' sets' next'
    let boxed = makeBoxes sets' vars compiledBody
    printfn "compileLambda: free %A boxed %A next %A" free boxed next
    collectFree free env <| Close (Seq.length free, boxed, next)

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

and compileAssign name rhs env s next =
    compileLookup name env
        (fun i -> compile rhs env s (AssignLocal (i, next)))
        (fun i -> compile rhs env s (AssignFree (i, next)))
        (fun i -> compile rhs env s (AssignGlobal (i, next)))
        (fun i -> failwith "compileAssign: cannot assign to predefined")

and compileBegin exprs env s next =
    let fold expr next = 
        compile expr env s next
    List.foldBack fold exprs next


/// VM
// Stack is universal storage with push, pop operation, and access to elements
let stack : obj [] = Array.create 1000 (new Object())
// Store is global storage where global variables stay
let globalStore : Value [] = Array.create 1000 Undefined
let predefinedStore : Value [] = Array.create 100 Undefined

let push value sp =
    Array.set stack sp value
    sp + 1

let stackGet sp i =
    printfn "<<<<<<<<<<<<<<<<<"
    for i in [0..sp] do
        printfn "stackGet: [%d] %A" i (Array.get stack i)
    printfn ">>>>>>>>>>>>>>>>>"
    Array.get stack (sp - i - 1)

let stackGetValue sp i = stackGet sp i :?> Value
let stackGetInstruction sp i = stackGet sp i :?> Instruction
let stackGetInt sp i = stackGet sp i :?> System.Int32 |> int

let stackSet sp i v =
    printfn "<<<<<<<<<<<<<<<<<"
    for i in [0..sp] do
        printfn "stackSet: [%d] %A" i (Array.get stack i)
    printfn ">>>>>>>>>>>>>>>>>"
    Array.set stack (sp - i - 1) v

let shiftArgs newCount oldCount sp =
    let rec loop i =
        if i >= 0 then
            stackSet sp (i + oldCount) (stackGet sp i)
            i - 1 |> loop
    loop (newCount - 1)
    sp - oldCount

let closure body n sp =
    let v = Array.create n Undefined
    let rec loop i =
        if i < n then
            Array.set v i (stackGetValue sp i)
    loop 0
    printfn "closure: %A %A" body v
    Closure (body, v)

let functionBody = function
    | Closure (body, _) -> body
    | PredefinedVal (a, b) -> PredefinedInstr (a, b)
    | _ -> failwith "closure expected"

let closureIndex clos i =
    match clos with 
    | Closure (_, v) -> 
        Array.get v i
    | _ -> failwith "closure expected"

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

let globalStoreGet i =
    Array.get globalStore i

let globalStoreSet i v =
    Array.set globalStore i v

let predefinedStoreGet i =
    Array.get predefinedStore i

let predefinedStoreSet i v =
    Array.set predefinedStore i v

let testNext accum theni elsei =
    if boolify accum
    then theni
    else elsei

let rec VM (accum : Value) expr frame clos sp =
    match expr with
    | Halt -> accum
    | ReferLocal (i, next) ->
        VM (stackGetValue frame i) next frame clos sp
    | ReferFree (i, next) ->
        VM (closureIndex clos i) next frame clos sp
    | ReferGlobal (i, next) ->
        VM (globalStoreGet i) next frame clos sp
    | ReferPredefined (i, next) ->
        VM (predefinedStoreGet i) next frame clos sp
    | Indirect next ->
        VM (unbox accum) next frame clos sp
    | Constant (n, next) ->
        VM (IntVal n) next frame clos sp
    | Close (n, body, next) ->
        VM (closure body n sp) next frame clos (sp - n)
    | Box (n, next) ->
        stackSet sp n (box (stackGetValue sp n))
        VM accum next frame clos sp
    | Test (theni, elsei) ->
        VM accum (testNext accum theni elsei) frame clos sp
    | AssignLocal (i, next) ->
        let refVal = stackGetValue frame i 
        printfn "assignLocal: %d refVal %A" i refVal
        assignRef accum refVal
        VM accum next frame clos sp
    | AssignFree (i, next) ->
        let refVal = closureIndex clos i 
        printfn "assignFree: %d refVal %A" i refVal
        assignRef accum refVal 
        VM accum next frame clos sp
    | AssignGlobal (i, next) ->
        globalStoreSet i accum
        VM accum next frame clos sp
    | Conti next ->
        VM (continuation sp) next frame clos sp
    | Nuate _ ->
        failwith "not implemented"
    | Frame (ret, next) ->
        VM accum next frame clos (push ret (push frame (push clos sp)))
    | Argument next ->
        VM accum next frame clos (push accum sp)
    | Shift (n, m, next) ->
        VM accum next frame clos (shiftArgs n m sp)
    | Apply ->
        VM accum (functionBody accum) sp accum sp
    | PredefinedInstr (arity, func) ->
        failwith ""
    | Return n ->
        let sp = sp - n
        VM accum (stackGetInstruction sp 0) (stackGetInt sp 1) (stackGetValue sp 2) (sp - 3)


/// Run untilities

let add sp =
    let n = stackGetInt sp 0
    let m = stackGetInt sp 1
    IntVal (n + m)

let predefined = [
    "+", 2, add
]
let globalVars = [
    "x"
    "y"
    "z"
]

let initEnv = [], Set.empty
let emptySets = Set.empty
let mutable emptyClosure = Closure (Halt, [||])

let evaluate instr =
    VM Undefined instr 0 emptyClosure 0

let prepareEnv () =
    for (name, arity, func) in predefined do
        let i = List.length globalEnv
        globalEnv <- List.append globalEnv [name]
        globalStoreSet i (PredefinedVal (arity, func))
    for name in globalVars do
        globalEnv <- List.append globalEnv [name]

let compileExpr expr =
    prepareEnv ()
    compile expr initEnv emptySets Halt

let stringToExpr s = stringToSExpr s |> sexprToExpr

let compileString s =
    let e = stringToSExpr s |> sexprToExpr
    compileExpr e

let evaluateString s =
    let expr = stringToSExpr s |> sexprToExpr
    evaluate (compileExpr expr)

let evaluateStringToString =
    evaluateString >> valueToString

/// Tests
let runTest input expected : unit=
    try
        let output = evaluateStringToString input
        if output <> expected then
            printfn "FAIL. input <%s>\nexpected<%s>\ngot<%s>" input expected output
        else ()
        ()
    with
    | e -> 
        if expected <> "***" then
            printfn "FAIL. Exception <%s>\ninput <%s>" e.Message input
        ()


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
let e4 = "(let ((x 1))
(set! x 321)
x)"
let e5 = "(begin
(set! x 123)
(let ((x 1))
    (set! x 321))
x)"
let e6 = "(((lambda (x) 
    (lambda (y) x)) 
    1) 2)"
let e7 = "(let ((f (lambda (x) x)))
(f 123)
(set! x 1234)
(f 9999))"
let e8 = "(let ((f (lambda (x) x)))
(f 9999))"
let e9 = "
(let ((f (lambda (x) x)))
    (let ((g (lambda (y) (f y))))
        (g 101010)
        (g 12345)))
"
let e10 = "
(let ((x 1)
      (f (lambda (g) (g 12345))))
    (let ((g (lambda (y) x)))
        (f g)))"
(* 
runTest "1" "1"
runTest "(if 1 2 3)" "2"
runTest "(if (if 1 2 3) 3 4)" "3"
runTest "(begin 1)" "1"
runTest "(begin 1 2 3)" "3"
runTest "(begin 2 (begin 3 4))" "4"
runTest "((((lambda (x abc) (lambda (y) (lambda (z) x))) 1 2) 3) 4)" "1"
runTest e4 "321"
runTest e5 "123"
runTest e6 "1"
*)

runTest "(+ 1 2)" "3"
compileString "(lambda (x) x)"
compileString "((lambda (x) x) 1)"
compileString e8
// evaluateString e8
evaluateString e10
compileString e10