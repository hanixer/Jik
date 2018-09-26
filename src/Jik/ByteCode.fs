module ByteCode

/// Expressions are converted to sequence of codes.
/// These code are then converted to byte representation
/// Then bytecode representation is run by interpreter.

open Base

type Value =
    | Void
    | Undefined
    | Nil
    | Int of int
    | Bool of bool
    | SymbValue of string
    | Cons of Value * Value
    | Function of Meaning
    | Closure of Meaning * Store
    | Primitive of (Value list -> Value)

and Store = Value array list

and Cont = Value -> Value

and Meaning = unit -> Value

// Environment determines position of variable in the runtime
// [a;b;c] -> [a;b] -> [x;y;z] -> NIL
type Env = string list list

type VarKind =
    | Local of 
        int * // index in environment
        int   // index inside environment entry
    | GlobalVar of int
    | PredefinedVar of int

type CodeEntry =
    | DeepArgumentRef of int * int
    | SetDeepArgument of int * int
    | CheckedGlobalRef of int
    | GlobalRef of int
    | SetGlobal of int
    | Predefined of int
    | Constant of Value
    | JumpFalse of int
    | Goto of int
    | UnlinkEnv
    | ExtendEnv
    | Call0 of int
    | Invoke1 of int
    | Invoke2 of int
    | Invoke3 of int
    | PushValue
    | PopArg1
    | PopArg2
    | ArityEq of int
    | ArithGt of int
    | PopFunction
    | CreateClosure of int
    | Return
    | PackFrame of int
    | FunctionInvoke
    | PreserveEnv
    | RestoreEnv
    | PopFrame of int
    | PopConsFrame of int
    | AllocateFrame of int
    | AllocateDottedFrame of int
    | Finish

let initialEnv = []

let rec valueToString = function
    | Int n -> sprintf "%d" n
    | Bool true -> sprintf "#t"
    | Bool false -> sprintf "#f"
    | Nil -> sprintf "()"
    | Function _ -> sprintf "Function"
    | Void -> "void"
    | Cons _ -> "cons"
    | SymbValue s -> s



let extendEnv env names =
    names :: env

let deepFetch store i j =
    match List.tryItem i store with 
    | Some it -> 
        printfn "deepFetch: item %s store %A" (Array.item j it |> valueToString) store
        Array.item j it
    | None -> failwithf "deepFetch: i <%d> j <%d> %A" i j store

let deepUpdate store i j value =
    match List.tryItem i store with 
    | Some a ->
        Array.set a j value
        value
    | None -> failwithf "deepUpdate: i <%d> j <%d>" i j

let extend store values =
    printfn "extend: values %A store %A" values store
    Array.ofList values :: store

type GlobalStore () =
    let values : Value array = Array.create 100 Undefined
    let bring n =
        values.Length + n
    member this.Fetch i =
        Array.get values i
    member this.Update i value =
        Array.set values i value
    
let globalStore = GlobalStore()
let predefinedStore = GlobalStore()

let mutable mainStore : Store = []

let tryFindLocal env name =
    let rec loopEnvEntry i j = function
        | [] -> None
        | n :: _ when name = n -> Local (i, j) |> Some
        | _ :: names -> loopEnvEntry i (j + 1) names
    let rec loopEnv i = function
        | [] -> None
        | entry :: env ->
            match loopEnvEntry i 0 entry with
            | None -> loopEnv (i + 1) env
            | success -> success
    loopEnv 0 env

type GlobalEnv (isPredefined) =
    let mutable level = 0
    let mutable vars : (string * int) list = []
    member this.TryFind (name) =
        match List.tryFind (fst >> ((=) name)) vars with
        | Some (_, i) -> 
            if isPredefined then
                PredefinedVar i |> Some
            else
                GlobalVar i |> Some
        | None -> None
    member this.Extend name =
        level <- level + 1
        vars <- (name, level) :: vars
        level
    override this.ToString() =
        sprintf "predefined? %b, %A" isPredefined vars
        

let globalEnv = GlobalEnv(false)
let predefinedEnv = GlobalEnv(true)

let tryComputeKind env name =
    match tryFindLocal env name with
    | Some local -> local |> Some
    | None ->
        match globalEnv.TryFind name with
        | Some glob -> glob |> Some
        | None ->
            predefinedEnv.TryFind name 

let computeKind env name =
    match tryComputeKind env name with
    | Some kind -> kind
    | _ -> 
        failwithf "computeKind: name '%s' not found\nglobal: %O\npredefined: %O" name globalEnv predefinedEnv

let initializeCurrent name =
    match tryComputeKind initialEnv name with
    | Some (GlobalVar i) ->
        globalStore.Update i Undefined
    | None ->
        let i = globalEnv.Extend name
        globalStore.Update i Undefined
    | _ -> failwithf "initializeCurrent: wrong redefinition"

let initializePredefined name value =
    match tryComputeKind initialEnv name with
    | Some (PredefinedVar i) ->
        predefinedStore.Update i value
    | None ->
        let i = predefinedEnv.Extend name
        predefinedStore.Update i value
    | _ -> failwithf "initializePredefined: wrong redefinition"

let boolify = function
    | Bool false -> false
    | _ -> true

let exprsToBeginForm exprs =
    SExpr.Cons (Symbol "begin", Base.exprsToList exprs)


/// Combinators

let DEEP_ARGUMENT_REF i j = fun () ->
    deepFetch mainStore i j

let CHECKED_GLOBAL_REF i = fun () ->
    match globalStore.Fetch(i) with
    | Undefined ->
        failwithf "CHECKED_GLOBAL_REF: Unitilizzed variable"
    | value ->
        value

let PREDEFINED i = fun () ->
    match predefinedStore.Fetch(i) with
    | Undefined ->
        failwithf "PREDEFINED: Unitilizzed variable"
    | value ->
        value

let CONSTANT value = fun () ->
    value

let IF cond conseq altern = fun () ->
    if cond () |> boolify then
        conseq ()
    else
        altern ()

let SEQUENCE curr next = fun () ->
    curr () |> ignore
    next ()

let DEEP_ARGUMENT_SET i j m = fun () ->
    let value = m ()
    deepUpdate mainStore i j value

let GLOBAL_SET i m = fun () ->
    let value = m ()
    globalStore.Update i value
    value

let FIX_CLOSURE m arity = fun () ->
    Closure (m, mainStore)

let CALL funcMeaning argsMeaning = fun () ->
    let funcValue = funcMeaning ()
    let argsValue = List.map (fun m -> m()) argsMeaning
    let savedStore = mainStore
    let result =
        match funcValue with
        | Closure (meaning, store) ->
            mainStore <- extend store argsValue
            meaning ()
        | Primitive f ->
            f argsValue
        | _ -> failwithf "wrong function value during call"
    mainStore <- savedStore
    result    

let callNumber =
    let mutable n = 0
    fun () ->
        n <- n + 1
        n

let rec compile env expr =
    match expr with
    | Number n ->  
        Int n |> Constant

let rec meaning env expr isTail =
    match expr with
    | Number n ->  Int n |> CONSTANT
    | Symbol name ->
        meaningRef env name isTail
    | List [Symbol "if"; cond; conseq; altern] ->
        meaningIf env cond conseq altern isTail
    | List (Symbol "begin" :: exprs) ->
        meaningBegin env exprs isTail
    | List [Symbol "set!"; Symbol name; rhs] ->
        meaningAssignment env name rhs isTail
    | List (Symbol "lambda" :: List args :: body) ->
        meaningLambda env args body isTail
    | List [Symbol "quote"; form] ->
        meaningQuote env form isTail
    | List (head :: tail) ->
        meaningApplication env head tail isTail
    | e -> failwith <| sexprToString e

and meaningIf env cond conseq altern isTail =
    let m1 = meaning env cond false
    let m2 = meaning env conseq isTail
    let m3 = meaning env altern isTail
    IF m1 m2 m3

and meaningRef env name isTail =
    match computeKind env name with
    | Local (i, j) ->
        DEEP_ARGUMENT_REF i j
    | GlobalVar i ->
        CHECKED_GLOBAL_REF i
    | PredefinedVar i ->
        PREDEFINED i

and meaningBegin env exprs isTail =
    let fold meaningNext expr = 
        let meaningCurr = meaning env expr false
        SEQUENCE meaningCurr meaningNext
    match List.rev exprs with
    | [] -> failwithf "meaningBegin: empty"
    | last :: prevs ->
        let m = meaning env last isTail
        List.fold fold m prevs

and meaningAssignment env name rhs isTail =
    let m = meaning env rhs false
    match computeKind env name with
    | Local (i, j) ->
        DEEP_ARGUMENT_SET i j m
    | GlobalVar i ->
        GLOBAL_SET i m
    | PredefinedVar i -> 
        failwithf "immutable predefined variable '%s'" name

and meaningLambda env args body isTail =
    let symToName = function
        | Symbol name -> name
        | _ -> failwithf "symToName: "
    let names = List.map symToName args
    let env' = extendEnv env names
    let bodyMeaning = meaningBegin env' body true
    FIX_CLOSURE bodyMeaning names.Length

and meaningApplication env head tail isTail =
    let funcMeaning = meaning env head false
    let argsMeaning = List.map (fun expr -> meaning env expr false) tail
    let num = callNumber()
    CALL funcMeaning argsMeaning

and meaningQuote env form isTail =
    match form with
    | Symbol name ->
        SymbValue name |> CONSTANT
    | List [] ->
         Nil |> CONSTANT
    | _ -> failwith "meaningQuote:"

/// Definitions
let plus cont =
    printfn "plus: store %A" mainStore
    match deepFetch mainStore 0 0, deepFetch mainStore 0 1 with
    | Int n, Int m -> n + m |> Int |> cont
    | _ -> failwith "+"

let numEqual cont =
    match deepFetch mainStore 0 0, deepFetch mainStore 0 1 with
    | Int n, Int m -> n = m |> Bool |> cont
    | _ -> failwith "="

let gt cont =
    match deepFetch mainStore 0 0, deepFetch mainStore 0 1 with
    | Int n, Int m -> n > m |> Bool |> cont
    | _ -> failwith ">"

let remainder cont =
    match deepFetch mainStore 0 0, deepFetch mainStore 0 1 with
    | Int n, Int m -> n % m |> Int |> cont
    | _ -> failwith "rema"

let cons cont =
    let v1 = deepFetch mainStore 0 0
    let v2 = deepFetch mainStore 0 1
    Cons (v1, v2)

let display cont =
    let v = deepFetch mainStore 0 0 
    printf "%s" <| valueToString v
    cont Void

let defprimitive name f =
    initializePredefined name (Primitive f)



defprimitive "+" <| fun values ->
    match values with
    | [Int n; Int m] -> n + m |> Int
    | _ -> failwithf "plus"
defprimitive ">" <| fun values ->
    match values with
    | [Int n; Int m] -> n > m |> Bool
    | _ -> failwithf "plus"

// defprimitive "=" numEqual
// defprimitive ">" gt
// defprimitive "remainder" remainder
// defprimitive "cons" cons
// defprimitive "display" display
initializePredefined "t" (Bool true)
initializePredefined "f" (Bool false)
initializePredefined "nil" Nil

initializeCurrent "x"
initializeCurrent "y"
initializeCurrent "z"
initializeCurrent "a"
initializeCurrent "b"
initializeCurrent "c"
initializeCurrent "max"
initializeCurrent "foo"
initializeCurrent "bar"
initializeCurrent "fib"
initializeCurrent "fact"
initializeCurrent "primes"
initializeCurrent "length"
initializeCurrent "filter"


/// Running
let evaluate (str : string) =
    let expr = stringToSExpr str
    let m = meaning [] expr true
    let mutable answer = Int 0
    let cont = fun value ->
        answer <- value
        value
    mainStore <- []
    m () 

let evaluateToString = evaluate >> valueToString