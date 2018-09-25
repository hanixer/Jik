module Optimized

/// Optimized interpreter. Here evaluation is split into two phases.
/// First - static phase, during which structure of call stack are
/// analyzed, so that during runtime variables could be addressed
/// by index, not by name. Result of this phase is called 'Meaning'
/// - a function that receives store (memory) and continuation 
/// and returns a value.
/// Second - dynamic phase is actually runtime. During this phase
/// store (memory) is used and can be changed. When function is
/// called store is extended with function's activation record.

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

and Store = Value array list

and Cont = Value -> Value

and Meaning = Store -> Cont -> Value

// Environment determines position of variable in the runtime
// [a;b;c] -> [a;b] -> [x;y;z] -> NIL
type Env = string list list

type VarKind =
    | Local of 
        int * // index in environment
        int   // index inside environment entry
    | Global of int
    | Predefined of int

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
    Array.ofList (List.rev values) :: store    

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
                Predefined i |> Some
            else
                Global i |> Some
        | None -> None
    member this.Extend name =
        level <- level + 1
        vars <- (name, level) :: vars
        level
        

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
    | _ -> failwithf "computeKind: name '%s' not found" name

let initializeCurrent name =
    match tryComputeKind initialEnv name with
    | Some (Global i) ->
        globalStore.Update i Undefined
    | None ->
        let i = globalEnv.Extend name
        globalStore.Update i Undefined
    | _ -> failwithf "initializeCurrent: wrong redefinition"

let initializePredefined name value =
    match tryComputeKind initialEnv name with
    | Some (Global i) ->
        predefinedStore.Update i value
    | None ->
        let i = predefinedEnv.Extend name
        predefinedStore.Update i value
    | _ -> failwithf "initializeCurrent: wrong redefinition"

let boolify = function
    | Bool false -> false
    | _ -> true

let exprsToBeginForm exprs =
    SExpr.Cons (Symbol "begin", Base.exprsToList exprs)

let applyFunction store cont funcValue values =
    let store' = extend store values
    match funcValue with
    | Function m ->
        m store' <| fun value ->
            printfn "function returned: %s" <| valueToString value
            cont value
    | _ -> failwithf "applyFunction: "

let rec meaning env expr =
    match expr with
    | Number n -> fun store cont -> Int n |> cont
    | Symbol name ->
        meaningRef env name
    | List [Symbol "if"; cond; conseq; altern] ->
        meaningIf env cond conseq altern
    | List (Symbol "begin" :: exprs) ->
        meaningBegin env exprs
    | List [Symbol "set!"; Symbol name; rhs] ->
        meaningAssignment env name rhs
    | List (Symbol "lambda" :: List args :: body) ->
        meaningLambda env args body
    | List [Symbol "quote"; form] ->
        meaningQuote env form
    | List (head :: tail) ->
        meaningApplication env head tail
    | e -> failwith <| sexprToString e

and meaningIf env cond conseq altern =
    let m1 = meaning env cond
    let m2 = meaning env conseq
    let m3 = meaning env altern
    fun store cont ->
        m1 store (fun value ->
            if boolify value then
                m2 store cont
            else
                m3 store cont)

and meaningRef env name =
    match computeKind env name with
    | Local (i, j) -> fun store cont ->
        deepFetch store i j |> cont
    | Global i -> fun store cont ->
        globalStore.Fetch i |> cont
    | Predefined i -> fun store cont ->
        predefinedStore.Fetch i |> cont

and meaningBegin env exprs =
    let fold meaningNext expr = 
        let meaningCurr = meaning env expr
        fun store cont ->
            meaningCurr store <| fun value ->
                meaningNext store cont
    match List.rev exprs with
    | [] -> failwithf "meaningBegin: empty"
    | last :: prevs ->
        let m = meaning env last
        List.fold fold m prevs

and meaningAssignment env name rhs =
    let m = meaning env rhs
    match computeKind env name with
    | Local (i, j) -> fun store cont ->
        m store <| fun value ->
            deepUpdate store i j value
            |> cont
    | Global i -> fun store cont ->
        m store <| fun value ->
            globalStore.Update i value
            cont value
    | Predefined i -> fun store cont ->
        m store <| fun value ->
            predefinedStore.Update i value
            cont value

and meaningLambda env args body =
    let symToName = function
        | Symbol name -> name
        | _ -> failwithf "symToName: "
    let names = List.map symToName args
    let env' = extendEnv env names
    let bodyMeaning = meaningBegin env' body
    fun store cont ->
        Function bodyMeaning |> cont

and meaningApplication env head tail =
    let funcMeaning = meaning env head
    let argsMeaning = List.map (meaning env) tail
    match List.rev argsMeaning with
    | m :: ms ->
        let funcValueRef = ref Undefined
        let valuesRef = ref []
        let lastArgMeaning store cont =
            m store <| fun value ->      
                let funcValue = !funcValueRef
                let values = value :: !valuesRef
                printfn "meaningApplication: lastArg value %s" <| valueToString value
                applyFunction store cont funcValue values
        let fold (next : Meaning) (curr : Meaning) =
            fun store cont ->
                curr store <| fun value ->
                    valuesRef := value :: !valuesRef
                    next store cont
        let argMeaning =
            List.fold fold lastArgMeaning ms
        fun store cont ->
            funcMeaning store <| fun funcValue ->
                funcValueRef := funcValue
                argMeaning store cont
    | [] -> fun store cont ->
        funcMeaning store <| fun funcValue ->
            applyFunction store cont funcValue []

and meaningQuote env form =
    match form with
    | Symbol name -> fun store cont ->
        SymbValue name |> cont
    | List [] -> fun store cont ->
        cont Nil
    | _ -> failwith "meaningQuote:"

/// Definitions
let plus store cont =
    match deepFetch store 0 0, deepFetch store 0 1 with
    | Int n, Int m -> n + m |> Int |> cont
    | _ -> failwith "+"

let numEqual store cont =
    match deepFetch store 0 0, deepFetch store 0 1 with
    | Int n, Int m -> n = m |> Bool |> cont
    | _ -> failwith "="

let gt store cont =
    match deepFetch store 0 0, deepFetch store 0 1 with
    | Int n, Int m -> n > m |> Bool |> cont
    | _ -> failwith ">"

let remainder store cont =
    match deepFetch store 0 0, deepFetch store 0 1 with
    | Int n, Int m -> n % m |> Int |> cont
    | _ -> failwith "rema"

let cons store cont =
    let v1 = deepFetch store 0 0
    let v2 = deepFetch store 0 1
    Cons (v1, v2)

let display store cont =
    let v = deepFetch store 0 0 
    printf "%s" <| valueToString v
    cont Void

let defprimitive name f =
    initializePredefined name (Function f)

defprimitive "+" plus
defprimitive "=" numEqual
defprimitive ">" gt
defprimitive "remainder" remainder
defprimitive "cons" cons
defprimitive "display" display
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
    let m = meaning [] expr
    let mutable answer = Int 0
    let cont = fun value ->
        answer <- value
        value
    m [] cont

let evaluateToString = evaluate >> valueToString