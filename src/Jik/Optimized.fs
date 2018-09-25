module Optimized

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

let extendEnv env names =
    names :: env

let deepFetch store i j =
    match List.tryItem i store with 
    | Some it -> Array.item j it
    | None -> failwithf "deepFetch: i <%d> j <%d>" i j

let deepUpdate store i j value =
    match List.tryItem i store with 
    | Some a ->
        Array.set a j value
        value
    | None -> failwithf "deepUpdate: i <%d> j <%d>" i j

let extend store values =
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

let boolify = function
    | Bool false -> false
    | _ -> true

let rec valueToString = function
    | Int n -> sprintf "%d" n
    | Bool true -> sprintf "#t"
    | Bool false -> sprintf "#f"
    | Nil -> sprintf "()"
    | Function _ -> sprintf "Function"
    | Void -> "void"
    | Cons _ -> "cons"
    | SymbValue s -> s

let exprsToBeginForm exprs =
    SExpr.Cons (Symbol "begin", Base.exprsToList exprs)

let applyFunction store cont funcValue values =
    let store' = extend store values
    match funcValue with
    | Function m ->
        m store' cont
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
        globalStore.Fetch i
    | Predefined i -> fun store cont ->
        predefinedStore.Fetch i

and meaningBegin env exprs =
    let fold meaningNext expr = 
        printfn "fold: "
        let meaningCurr = meaning env expr
        fun store cont ->
            printfn "begin: meaning: %s" <| sexprToString expr
            meaningCurr store <| fun value ->
                printfn "begin: cont"
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
            printfn "localset: "    
            deepUpdate store i j value
            |> cont
    | Global i -> fun store cont ->
        m store <| fun value ->
            printfn "global: %s" <| valueToString value
            globalStore.Update i value
            cont value
    | Predefined i -> fun store cont ->
        m store <| fun value ->
            printfn "predefined: "
            predefinedStore.Update i value
            cont value

and meaningLambda env args body =
    let symToName = function
        | Symbol name -> name
        | _ -> failwithf "symToName: "
    let names = List.map symToName args
    let env' = extendEnv env names
    let bodyMeaning = meaningBegin env' body
    bodyMeaning

and meaningApplication env head tail =
    let funcMeaning = meaning env head
    let argsMeaning = List.map (meaning env) tail
    fun store cont ->
        printfn "use funcMeaning"
        funcMeaning store <| fun funcValue ->
            printfn "funcValue retrieved"
            argsMeaning.[0] store <| fun value1 ->
                printfn "value1"
                argsMeaning.[1] store <| fun value2 ->
                    printfn "value2"
                    applyFunction store cont funcValue [value1; value2]


/// Definitions
initializeCurrent "x"


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