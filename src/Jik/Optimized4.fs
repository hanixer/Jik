module Optimized4

/// Change representation of meaning by using combinators.
/// Now meaning is just a function : () -> Value

open Base
open System.Numerics

type Value =
    | Void
    | Undefined
    | Nil
    | Int of int
    | Bool of bool
    | SymbValue of string
    | Cons of Value * Value


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


/// Combinators

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


let GLOBAL_SET i m = fun () ->
    let value = m ()
    globalStore.Update i value
    value

/// Running
type Ip = (unit -> unit) option

type Cont =
    | IfCont of SExpr * SExpr * Cont
    | BeginCont of SExpr list * Cont
    | AssignCont of string * Cont
    | Halt

let mutable value : Value = Int 0
let mutable ip : Ip = None
let mutable cont : Cont = Halt
let mutable env : BigInteger = BigInteger.MinusOne
let mutable expr : SExpr = SExpr.Nil


type BeginCont =
    { Rest : SExpr list
      Cont : Cont }

let rec ev () =
    match expr with
    | Number n ->
        constant (Int n)
    | Symbol name ->
        constant (SymbValue name)
    | List [Symbol "if"; cond; conseq; altern] ->
        cont <- IfCont (conseq, altern, cont)
        expr <- cond
    | List (Symbol "begin" :: e :: exprs) ->
        cont <- BeginCont (exprs, cont)
        expr <- e
    | List [Symbol "set!"; Symbol name; rhs] ->
        failwith "" //
    | List (Symbol "lambda" :: List args :: body) ->
        failwith "" //
    | List [Symbol "quote"; form] ->
        failwith "" //
    | List (head :: tail) ->
        failwith "" //
    | e -> failwith <| sexprToString e

and constant v =
    value <- v
    ip <- Some applyCont

and applyCont () =
    match cont with
    | Halt ->
        ip <- None
    | IfCont (conseq, altern, savedCont) ->
        cont <- savedCont
        if boolify value then
            expr <- conseq
        else
            expr <- altern
        ip <- Some ev
    | BeginCont (exprs, savedCont) ->
        match exprs with
        | e :: rest ->
            cont <- BeginCont (rest, savedCont)
            expr <- e
            ip <- Some ev
        | [] ->
            cont <- savedCont

let evaluate (str : string) =
    let e = stringToSExpr str
    let rec loop () =
        match ip with
        | Some func -> 
            func ()
            loop ()
        | None ->
            ()
    value <- Undefined
    ip <- Some ev
    cont <- Halt
    expr <- e
    loop ()
    value

let evaluateToString = evaluate >> valueToString