module ContinuationInterpreter

open Base

type Value =
    | Int of int
    | Function of string list * SExpr * Env
    | Primitive of (Env -> (Value -> ContResult) -> Value list -> ContResult)
    | Void
    | Bool of bool
    | Nil
    | Cons of Value * Value
    | SymbValue of string

and EnvEntry = 
    | BlockEntry of Cont
    | ValueEntry of Value ref

and Env = Map<string, EnvEntry>

and Cont = Value -> ContResult

and ContResult = unit

let rec valueToString = function
    | Int n -> sprintf "%d" n
    | Bool true -> sprintf "#t"
    | Bool false -> sprintf "#f"
    | Nil -> sprintf "()"
    | Function (ns, _, _) -> sprintf "Function %A" ns
    | Void -> "void"
    | Cons _ -> "cons"
    | Primitive _ -> "Primitive"
    | SymbValue s -> s

let mutable catchStack : (Value * Cont) list = []

let extendEnv names values (env : Env) =
    let values = List.map ref values
    List.zip names values
    |> List.fold (fun acc (name, value) ->
        Map.add name (ValueEntry value) acc) env

let extendEnvValue name value (env : Env) =
    let value = ref value
    Map.add name (ValueEntry value) env

let extendEnvBlock name k (env : Env) =
    Map.add name (BlockEntry k) env

let lookupValue env name =
    match Map.tryFind name env with
    | Some (ValueEntry v) -> v
    | _ -> failwithf    "lookupValue: name <%s> was not found" name

let lookupBlock env name =
    match Map.tryFind name env with
    | Some (BlockEntry k) -> k
    | _ -> failwithf    "lookupValue: name <%s> was not found" name

let exprsToBeginForm exprs =
    SExpr.Cons (Symbol "begin", Base.exprsToList exprs)

let resume cont v =
    cont v

let mkIfCont eval k t e = function
    | Int 0 | Bool false -> eval k e
    | _ -> eval k t

let rec mkBeginCont eval k exprs = fun value ->
    match exprs with
    | [] -> resume k value
    | expr :: exprs ->
        let cont = mkBeginCont eval k exprs
        eval cont expr

let mkSetCont env k name = fun value ->
    let reference = lookupValue env name
    reference := value
    resume k value

let applyFunction eval env k func values =
    printfn "applyFunction: %A" (List.map valueToString values)
    match func with
    | Function (names, body, funcEnv) ->
        let env = extendEnv names values funcEnv
        eval env k body
    | Primitive f -> f env k values
    | e -> failwithf "applyFunction: function expected, got %A" e

let rec mkArgCont eval env k func values exprs = fun value ->
    printfn "mkArgCont: %s" (valueToString value)
    let values = value :: values
    match exprs with
    | [] -> applyFunction eval env k func (List.rev values)
    | expr :: exprsRest ->
        let cont = mkArgCont eval env k func values exprsRest
        eval env cont expr

let mkAppCont eval env k exprs = fun value ->
    printfn "mkAppCont: %s" (valueToString value)
    match exprs with
    | [] -> applyFunction eval env k value []
    | first :: args ->
        let cont = mkArgCont eval env k value [] args
        eval env cont first

let processLambda env k names body =
    let names = 
        List.map 
            (function Symbol n -> n | _ -> failwith "wrong") 
            names
    Function (names, body, env) |> resume k

let findCatchStackEntry value =
    let f (v2,_) = 
        valueToString v2 = valueToString value
    match List.tryFind f catchStack with
    | Some (_, k) -> k
    | None -> failwithf "findCatch: not found"

let throwCont eval env k body = fun value ->
    let catchCont = findCatchStackEntry value
    eval env catchCont body

let catchCont eval env k body = fun value ->
    catchStack <- (value, k) :: catchStack
    eval env k body

let evalReturnFrom eval env mark body =
    printfn "evalReturnFrom: %s" mark
    let foundCont = lookupBlock env mark    
    eval env foundCont body

let evalBlockBody eval env k mark body = 
    let env = extendEnvBlock mark k env
    eval env k body

let rec evaluate env k expr = 
    let eval k expr = evaluate env k expr
    match expr with
    | Symbol n -> 
        !(lookupValue env n) |> resume k
    | Number n -> Int n |> resume k
    | List [Symbol "quote"; Symbol s] ->
        SymbValue s |> resume k
    | List [Symbol "if"; c; t; e] ->
        let cont = mkIfCont eval k t e
        eval cont c
    | List (Symbol "begin" :: exprs) ->
        let cont = mkBeginCont eval k exprs
        cont (Int 0)
    | List [Symbol "set!"; Symbol name; rhs] ->
        let cont = mkSetCont env k name
        eval cont rhs
    | List (Symbol "catch" :: mark :: body) ->
        let body = exprsToBeginForm body
        let cont = catchCont evaluate env k body
        eval cont mark
    | List [Symbol "throw"; mark; body] ->
        let cont = throwCont evaluate env k body
        eval cont mark
    | List (Symbol "block" :: Symbol mark :: body) ->
        exprsToBeginForm body
        |> evalBlockBody evaluate env k mark
    | List [Symbol "return-from"; Symbol mark; body] ->
        evalReturnFrom evaluate env mark body
    | SExpr.Cons (Symbol "lambda", SExpr.Cons (List names, body)) ->
        processLambda env k names (SExpr.Cons (Symbol "begin", body))
    | List (expr :: exprs) ->
        let cont = mkAppCont evaluate env k exprs
        eval cont expr

let power env k values =
    List.fold (fun n v ->
        match v with
        | Int m -> n * m
        | _ -> failwith "power: invalid") 1 values
    |> Int
    |> resume k


/// Setting up initial environment
let getInitialEnv () =
    let mutable initialEnv : Env = Map.empty

    let definitial s value =
        initialEnv <- extendEnvValue s value initialEnv

    let definitial0 s =
        initialEnv <- extendEnvValue s Void initialEnv

    let defprimitive name func arity = 
        let f = fun env k values ->
            if arity = List.length values then
                func values |> resume k
            else
                failwithf "defprimitive: wrong arity, name <%s>, expected <%d>, got <%d>" name arity (List.length values)
        definitial name (Primitive f)

    let defprimitive1 name func =
        defprimitive name (function
            | [x] -> func x
            | _ -> failwith "defprimitive1") 1

    definitial "t" (Bool true)
    definitial "f" (Bool false)
    definitial "nil" Nil

    definitial0 "x"
    definitial0 "y"
    definitial0 "z"
    definitial0 "a"
    definitial0 "b"
    definitial0 "c"
    definitial0 "foo"
    definitial0 "bar"
    definitial0 "hux"
    definitial0 "fib"
    definitial0 "fact"
    definitial0 "visit"
    definitial0 "length"
    definitial0 "primes"

    defprimitive "cons" (function
        | [x;y] -> Cons (x,y)) 2
    defprimitive1 "car" (function
        | Cons (x, _) -> x
        | _ -> failwith "car")

    let callcc env k values =
        match values with
        | [value] ->
            let cont = fun env _ exprs ->
                List.head exprs |> resume k
            applyFunction evaluate env k value [Primitive cont]
        | _ -> failwithf "callcc: wrong arity"
    definitial "call/cc" (Primitive callcc)

    definitial "*" (Primitive power)

    initialEnv


/// Top level functions
let evaluateString s =
    let initialEnv = getInitialEnv()
    let sexpr = stringToSExpr s
    let mutable answer = ""
    let cont = fun value ->
        answer <- valueToString value
    catchStack <- []
    evaluate initialEnv cont sexpr
    answer