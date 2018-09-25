module Optimized2

/// In this version runtime environment is held 
/// by a global variable 'mainStore'.
/// Tail call optimization is implemented by
/// modifying 'mainStore' variable.
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

and Meaning = Cont -> Value

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
    printfn "extend: values %A store %A" values store
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
                Predefined i |> Some
            else
                Global i |> Some
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
    | _ -> failwithf "initializePredefined: wrong redefinition"

let boolify = function
    | Bool false -> false
    | _ -> true

let exprsToBeginForm exprs =
    SExpr.Cons (Symbol "begin", Base.exprsToList exprs)

let applyFunction cont funcValue values isTail =
    mainStore <- extend mainStore values
    match funcValue with
    | Function m ->
        if not isTail then
            let store = mainStore
            m <| fun value ->
                printfn "function returned: %s" <| valueToString value
                mainStore <- store
                cont value
        else
            m <| fun value ->
                printfn "function returned: %s" <| valueToString value
                cont value
    | _ -> failwithf "applyFunction: "

let rec meaning env expr isTail =
    match expr with
    | Number n -> fun cont -> Int n |> cont
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
    fun cont ->
        m1 <| fun value ->
            if boolify value then
                m2 cont
            else
                m3 cont

and meaningRef env name isTail =
    match computeKind env name with
    | Local (i, j) -> fun cont ->
        deepFetch mainStore i j |> cont
    | Global i -> fun cont ->
        globalStore.Fetch i |> cont
    | Predefined i -> fun cont ->
        predefinedStore.Fetch i |> cont

and meaningBegin env exprs isTail =
    let fold meaningNext expr = 
        let meaningCurr = meaning env expr false
        fun cont ->
            meaningCurr <| fun value ->
                meaningNext cont
    match List.rev exprs with
    | [] -> failwithf "meaningBegin: empty"
    | last :: prevs ->
        let m = meaning env last isTail
        List.fold fold m prevs

and meaningAssignment env name rhs isTail =
    let m = meaning env rhs false
    match computeKind env name with
    | Local (i, j) -> fun cont ->
        m <| fun value ->
            deepUpdate mainStore i j value
            |> cont
    | Global i -> fun cont ->
        m <| fun value ->
            globalStore.Update i value
            cont value
    | Predefined i -> fun cont ->
        m <| fun value ->
            predefinedStore.Update i value
            cont value

and meaningLambda env args body isTail =
    let symToName = function
        | Symbol name -> name
        | _ -> failwithf "symToName: "
    let names = List.map symToName args
    let env' = extendEnv env names
    let bodyMeaning = meaningBegin env' body true
    fun cont ->
        Function bodyMeaning |> cont

and meaningApplication env head tail isTail =
    let funcMeaning = meaning env head false
    let argsMeaning = List.map (fun expr -> meaning env expr false) tail
    match List.rev argsMeaning with
    | m :: ms ->
        let funcValueRef = ref Undefined
        let valuesRef = ref []
        let lastArgMeaning cont =
            m <| fun value ->      
                let funcValue = !funcValueRef
                let values = value :: !valuesRef
                printfn "call apply function %A" values
                applyFunction cont funcValue values isTail
        let fold (next : Meaning) (curr : Meaning) =
            fun cont ->
                curr <| fun value ->
                    valuesRef := value :: !valuesRef
                    next cont
        let argMeaning =
            List.fold fold lastArgMeaning ms
        fun cont ->
            funcMeaning <| fun funcValue ->
                funcValueRef := funcValue
                argMeaning cont
    | [] -> fun cont ->
        funcMeaning <| fun funcValue ->
            printfn "call apply function %A" []
            applyFunction cont funcValue [] isTail

and meaningQuote env form isTail =
    match form with
    | Symbol name -> fun cont ->
        SymbValue name |> cont
    | List [] -> fun cont ->
        cont Nil
    | _ -> failwith "meaningQuote:"

/// Definitions
let plus cont =
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
    let m = meaning [] expr true
    let mutable answer = Int 0
    let cont = fun value ->
        answer <- value
        value
    mainStore <- []
    m cont 

let evaluateToString = evaluate >> valueToString