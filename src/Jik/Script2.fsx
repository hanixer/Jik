#load "Base.fs"
#load "Util.fs"
#load "Display.fs"

/// Supported features: 
/// - core Scheme
/// - let
/// - letrec
/// - integer, booleans, pairs
/// - set!
/// - call/cc
/// - tail calls
/// - inline primitives (like +, -, etc.)
/// This interpreter translates s-expressions into core Scheme making 
/// some desugaring (let, letrec). Then core expression is translated
/// into Tree-like instructions for ''VM''.
/// VM has following state:
/// - accumulator, holding result value 
/// - next instruction to evaluate - emulating instruction pointer
/// - frame pointer - pointer to beginning of the current frame
/// - closure - which contains code and free variables for current executing function.
/// - stack pointer - which points to next free location on the stack.
/// Memory organization. 
/// Predefined storage - where primitives are stored.
/// Global storage - where global variables are store.
/// Stack memory - implemented as an array, supporting push, pop.
/// Each closure has its own area of memory for free variables.
/// call/cc is implemented by saving all active stack elements and then
/// restoring them back as needed.

open System
open Base
open Display
open System.Text

type Expr =
    | Int of int
    | Bool of bool
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

and findModifiedVarsList vars exprList =
    List.map (findModifiedVars vars) exprList
    |> Set.unionMany

let findModiedVarsInBody free argVars body =
    let sets = findModifiedVarsList (Set.ofSeq argVars) body 
    Set.union sets (Set.intersect sets (Set.ofSeq free))

let rec sexprToExpr sexpr =
   match sexpr with
    | SExpr.Number n ->
        Int n
    | SExpr.Symbol name ->
        Ref name
    | SExpr.Bool b ->
        Bool b
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
    | List (Symbol "letrec" :: List bindings :: body) ->
        let bindings = transformLetrecBindings bindings
        let fnames = List.map fst bindings
        let bindInitial fname = 
            Cons (Symbol fname, Cons (Number 0, Nil))
        let letBindings = 
            fnames
            |> List.map bindInitial
            |> exprsToList
        let foldSets (name, (args, lamBody)) next =
            let args = List.map Symbol args |> exprsToList
            let lambda = exprsToList [Symbol "lambda"; args; lamBody]
            let set = exprsToList [Symbol "set!"; Symbol name; lambda]
            set :: next
        let body =
            List.foldBack foldSets bindings body
        exprsToList (Symbol "let" :: letBindings :: body)
        |> sexprToExpr
    | List [Symbol "call/cc"; sexpr] ->
        Callcc (sexprToExpr sexpr)
    | List (head :: tail) ->
        App (sexprToExpr head, List.map sexprToExpr tail)
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
    | ConstantInt of int * Instruction
    | ConstantBool of bool * Instruction
    | Return of int
    | Close of int * Instruction * Instruction
    | Test of Instruction * Instruction
    | Apply
    | PredefinedInstr of int * Instruction
    | Conti of Instruction
    | Frame of Instruction * Instruction
    | Argument of Instruction
    | Indirect of Instruction
    | Shift of int * int * Instruction
    | Halt
    | Box of int * Instruction
    | Nuate of (obj []) * Instruction

and Value =
    | NilVal
    | IntVal of bigint
    | BoolVal of bool
    | ConsVal of Value * Value
    | Closure of Instruction * (Value [])
    | PredefinedVal of int * PredefinedFunc
    | Boxed of Value ref
    | Undefined

and PredefinedFunc = (int -> Value)

let mutable globalEnv : string list = []
let mutable predefinedEnv : string list = []

let rec valueToString = function
    | NilVal -> "()"
    | IntVal n -> (n.ToString())
    | BoolVal true -> "#t"
    | BoolVal false -> "#f"
    | ConsVal (x, y) as e-> 
        printfn "cons cons : %A" e
        let sb = new StringBuilder()
        sb.Append("(").Append(valueToString x) |> ignore
        let rec loop = function
            | ConsVal (x, NilVal) ->
                sb.Append(valueToString x) |> ignore
            | ConsVal (x, (ConsVal (_, _) as y)) ->
                sb.Append(valueToString x).Append(" ") |> ignore
                loop y
            | ConsVal (x, y) -> 
                sb.Append(valueToString x).Append(" . ").Append(valueToString y) |> ignore
            | _ -> ()
        loop y
        sb.Append(")") |> ignore
        sb.ToString()
    | Boxed r -> sprintf "<box %s>" (valueToString !r)
    | Closure _ -> "<closure>"
    | Undefined -> "<undefined>"
    | PredefinedVal(_, _) -> failwith "Not Implemented"

let isTail = function
    | Return _ -> true
    | _ -> false

let tryFindPredefinedIndex expr =
    match expr with
    | Ref name ->
        List.tryFindIndex ((=) name) predefinedEnv
    | _ -> None

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

let freeVarsInBody vars body =
    let bound = Set.union (Set.ofSeq vars) (Set.ofSeq predefinedEnv)
    let s = freeVarsExprList bound body
    let result = Set.toSeq s
    result

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
    | Int n -> ConstantInt (n, next)
    | Bool n -> ConstantBool (n, next)
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
    collectFree free env <| Close (Seq.length free, boxed, next)

and compileApp func argExprs env s next =
   match tryFindPredefinedIndex func with
    | Some i ->
       compilePredefined i argExprs env s next
    | None ->
       compileFunc func argExprs env s next

and compilePredefined i argExprs env s next =
    compileArgs argExprs env s <| PredefinedInstr (i, next)

and compileFunc func argExprs env s next = 
    match next with
    | Return n -> 
        let next' = Shift (List.length argExprs, n, Apply)
        let compiledFunc = compile func env s next'
        compileArgs argExprs env s compiledFunc
    | _ -> 
        let next' = Apply
        let compiledFunc = compile func env s next'        
        Frame (next, compileArgs argExprs env s compiledFunc)

and compileArgs argExprs env s compiledFunc =
    let compileArg next arg =
        compile arg env s (Argument next)
    List.fold compileArg compiledFunc argExprs

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
    match Array.tryItem (sp - i - 1) stack with
    | Some x -> x
    | _ -> failwithf "stackGet sp=%d i=%d" sp i

let stackGetValue sp i = stackGet sp i :?> Value
let stackGetInstruction sp i = stackGet sp i :?> Instruction
let stackGetInt sp i = stackGet sp i :?> System.Int32 |> int

let stackSet sp i v =
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
    Closure (body, v)

let handleApply accum sp =
    match accum with
    | Closure (body, _) -> accum, body
    | PredefinedVal (arity, func) ->
        func sp, Return arity
    | Boxed r ->
        match !r with
        | Closure (body, _) -> accum, body
        | e -> failwithf "closure expected, got BOXED %A" e
    | e -> failwithf "closure expected, got %A" e

let closureIndex clos i =
    match clos with 
    | Closure (_, v) | Boxed {contents = Closure (_, v)} -> 
        Array.get v i
    | e -> failwithf "closureIndex: closure expected, got %A" e

let unbox = function
    | Boxed v -> !v
    | _ -> failwith "unbox: not a box"

let box v = Boxed (ref v)

let boolify = function
    | BoolVal false -> false
    | _ -> true

let assignRef v = function
    | Boxed r ->
        r := v
    | _ -> failwith "assignRef: "

let saveStack sp =
    Array.sub stack 0 sp 

let restoreStack savedStack =
    let length = Array.length savedStack
    for i in [0..length - 1] do
        Array.set stack i (Array.get savedStack i)
    length

let continuation sp =
    let savedStack = saveStack sp
    closure (ReferLocal (0, Nuate (savedStack, Return 0))) 0 sp

let globalStoreGet i =
    Array.get globalStore i

let globalStoreSet i v =
    Array.set globalStore i v

let predefinedStoreGet i =
    Array.get predefinedStore i

let predefinedStoreSet i v =
    Array.set predefinedStore i v

let getPredefinedFunc i =
    match predefinedStoreGet i with
    | PredefinedVal (_, func) -> func
    | _ -> failwithf "getPredefinedFunc: %d" i

let getPredefinedArity i =
    match predefinedStoreGet i with
    | PredefinedVal (arity, _) -> arity
    | _ -> failwithf "getPredefinedArity: %d" i

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
    | ConstantInt (n, next) ->
        VM (IntVal <| bigint n) next frame clos sp
    | ConstantBool (b, next) ->
        VM (BoolVal b) next frame clos sp
    | Close (n, body, next) ->
        VM (closure body n sp) next frame clos (sp - n)
    | Box (n, next) ->
        stackSet sp n (box (stackGetValue sp n))
        VM accum next frame clos sp
    | Test (theni, elsei) ->
        VM accum (testNext accum theni elsei) frame clos sp
    | AssignLocal (i, next) ->
        let refVal = stackGetValue frame i 
        assignRef accum refVal
        VM accum next frame clos sp
    | AssignFree (i, next) ->
        let refVal = closureIndex clos i 
        assignRef accum refVal 
        VM accum next frame clos sp
    | AssignGlobal (i, next) ->
        globalStoreSet i accum
        VM accum next frame clos sp
    | Frame (ret, next) ->
        VM accum next frame clos (push ret (push frame (push clos sp)))
    | Argument next ->
        VM accum next frame clos (push accum sp)
    | Shift (n, m, next) ->
        VM accum next frame clos (shiftArgs n m sp)
    | Apply ->
        let accum, next = handleApply accum sp
        VM accum next sp accum sp
    | Return n ->
        let sp = sp - n
        VM accum (stackGetInstruction sp 0) (stackGetInt sp 1) (stackGetValue sp 2) (sp - 3)
    | PredefinedInstr (i, next) ->
        VM (getPredefinedFunc i sp) next frame clos (sp - (getPredefinedArity i))
    | Conti next ->
        VM (continuation sp) next frame clos sp
    | Nuate (savedStack, next) ->
         VM accum next frame clos (restoreStack savedStack)

/// Run untilities

let arithm func sp =
    match stackGetValue sp 0, stackGetValue sp 1 with
    | IntVal n, IntVal m ->
        IntVal (func n m)
    | _ -> failwith "arithm: wrong values"

let compar func sp =
    match stackGetValue sp 0, stackGetValue sp 1 with
    | IntVal n, IntVal m ->
        BoolVal (func n m)
    | _ -> failwith "compar: wrong values"

let cons sp =
    ConsVal (stackGetValue sp 0, stackGetValue sp 1)

let pairHelper func sp =
    match stackGetValue sp 0 with
    | ConsVal (x, y) -> func x y
    | _ -> failwith "pairHelper: cons expected"

let car = pairHelper (fun x _ -> x)
let cdr = pairHelper (fun _ y -> y)

let isPair sp = 
    match stackGetValue sp 0 with
    | ConsVal (_, _) -> BoolVal true
    | _ -> BoolVal false

let predefined = [
    "+", 2, arithm (+)
    "-", 2, arithm (-)
    "*", 2, arithm (*)
    "/", 2, arithm (/)
    "<", 2, compar (<)
    "cons", 2, cons
    "car", 1, car
    "cdr", 1, cdr
    "pair?", 1, isPair
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
    predefinedEnv <- []
    globalEnv <- []
    for (name, arity, func) in predefined do
        let i = List.length predefinedEnv
        predefinedEnv <- List.append predefinedEnv [name]
        predefinedStoreSet i (PredefinedVal (arity, func))
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

/// Pretty-printing
let rec showInstruction instr =
    let withNumber str n next = 
        iConcat [iStr str; iNum n; iNewline; showInstruction next]
    match instr with
    | Halt -> iStr "Halt"
    | ReferLocal(n, next) -> withNumber "ReferLocal " n next
    | ReferFree(n, next) -> withNumber "ReferFree " n next
    | ReferGlobal(n, next) -> withNumber "ReferGlobal " n next
    | ReferPredefined(n, next) -> withNumber "ReferPredefined " n next
    | AssignLocal(n, next) -> withNumber "AssignLocal " n next
    | AssignFree(n, next) -> withNumber "AssignFree " n next
    | AssignGlobal(n, next) -> withNumber "AssignGlobal " n next
    | ConstantInt(n, next) -> withNumber "Constant " n next
    | ConstantBool(n, next) -> 
        let x = if n then iStr "#t" else iStr "#f"
        iConcat [iStr "ConstantBool "; x; iNewline; showInstruction next]
    | Return(n) -> 
        iConcat [iStr "Return "; iNum n; iNewline]
    | Close(freeCount, body, next) -> 
        iConcat [iStr "Close free="; iNum freeCount; iIndent (iConcat [iNewline; showInstruction body]); iNewline; showInstruction next]
    | Test(t, e) ->
        iConcat [iStr "Test"; iNewline; iStr "then"; iNewline; showInstruction t; iNewline; iStr "else"; iNewline; showInstruction e]
    | Apply -> iStr "Apply"
    | Frame(ret, body) ->
        iConcat [iStr "Frame"; 
                 iIndent (iConcat [iNewline; showInstruction ret; iNewline; iStr "---"; iNewline; showInstruction body])]
    | Argument(next) -> 
        iConcat [iStr "Argument"; iNewline; showInstruction next]
    | Indirect(next) ->
        iConcat [iStr "Indirect"; iNewline; showInstruction next]
    | Shift(newCount, oldCount, next) ->
        iConcat [iStr "Shift new="; iNum newCount; iStr ", old="; iNum oldCount; iNewline; showInstruction next]
    | Box(n, next) -> withNumber "Box " n next
    | Conti(next) -> 
        iConcat [iStr "Conti"; iNewline; showInstruction next]
    | Nuate(_, _) -> 
        iConcat [iStr "Nuate"; ]//iNewline; showInstruction next]
    | PredefinedInstr(n, next) -> withNumber "PredefinedInstr " n next

let printInstruction instr = 
    showInstruction instr |> iDisplay |> printf "%s"

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
let e8_ = "(let ((f (lambda (x) x)))
(f 1234)
(f 4444)
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
let e11 = "
(let ((f +)) 
    (let ((g f))
        (g 1 2)))"
let e12 = "(call/cc (lambda (k) (k 2) 3))"
let e13 = "(call/cc (lambda (k) (+ 1 (k 12))))"
let e14 = "
(letrec ((fact (lambda (x) 
    (if (< x 2)
        x
        (* x (fact (- x 1)))))))
  (fact 5))"
let e16 = "
(letrec ((fact (lambda (x) 
    (if (< x 2)
        x
        (* x (fact (- x 1)))))))
  (fact 50))"
let e15 = "
(letrec (
    (g (lambda (x) (f x)))
    (f (lambda (x) (+ 123 x))))
  (g 100))"
(* 
// Basic tests
runTest "1" "1"
runTest "(if 1 2 3)" "2"
runTest "(if (if 1 2 3) 3 4)" "3"
runTest "(begin 1)" "1"
runTest "(begin 1 2 3)" "3"
runTest "(begin 2 (begin 3 4))" "4"
// Procedures
runTest "((((lambda (x abc) (lambda (y) (lambda (z) x))) 1 2) 3) 4)" "1"
// Let
runTest e4 "321"
runTest e5 "123"
runTest e6 "1"
// Predefined
runTest "(+ 1 2)" "3"
runTest "(+ (+ 1 2) (+ 3 4))" "10"
runTest "(let ((f +)) (f 1 2))" "3"
runTest e11 "3"
// Callcc
runTest e12 "2"
runTest e13 "12"
// Letrec, recursive
runTest e14 "120"
// Lists
runTest "(pair? (cons 1 2))" "#t"
runTest "(pair? #t)" "#f"
runTest "(cons 1 2)" "(1 . 2)"
runTest "(cons 1 ())" "(1)"
runTest "(cons 1 (cons 2 3))" "(1 2 . 3)"
runTest "(cons 1 (cons (cons 2 ()) (cons 3 ())))" "(1 (2) 3)"
runTest "(car (cons 1 2))" "1"
runTest "(cdr (cons 1 2))" "2"
runTest "(cdr (car (cdr (cons 1 (cons (cons 2 ()) (cons 3 ()))))))" "()"

*)