module Conversion

open Base

// <exp> ::= <const>
//        |  

type Var =
    | GlobalVar of string
    | PredefinedVar of string * string
    | LocalVar of string * bool ref * bool

type Ref = Var

type Program =
    | Ref of Ref
    | LocalAssignment of Ref * Program
    | GlobalAssignment of Var * Program
    | BoxRead of Ref
    | BoxWrite of Ref * Program
    | BoxCreation of Var
    | Function of Var list * Program
    | Alternative of Program * Program * Program
    | Sequence of Program list
    | Constant of int
    | RegularApplication of Program * Program list
    | PredefinedApplication of string * Program list
    | FixLet of Var list * Program list * Program

type Env = Var list

let isVariableDotted = function
    | LocalVar (_, _, true) -> true
    | _ -> false

let mutable globalEnv : Env = []
let mutable predefinedEnv : Env = []

let tryFindVariable name env =
    let isGood = function
        | GlobalVar n -> name = n
        | PredefinedVar (n, _) -> name = n
        | LocalVar (n, _, _) -> name = n
    List.tryFind isGood env
    
let findVariableInAllEnvs name env =
    match tryFindVariable name env with
    | Some v -> v
    | _ ->
        match tryFindVariable name globalEnv with
        | Some v -> v
        | _ -> 
            match tryFindVariable name predefinedEnv with
            | Some v -> v
            | _ -> failwithf "findVariableInAllEnvs: variable '%s' not found"  name

let getNames = function
    | Symbol n -> [LocalVar (n, ref false, true)]
    | List names ->
        List.map (function
            | Symbol n -> LocalVar (n, ref false, false)
            | _ -> failwith "getNames: Symbol expected")
            names
    | _ -> failwith "getNames: Symbol or List of Symbols are expected"

let convertReference name env =
    findVariableInAllEnvs name env
    |> Ref

let convertAssignment variable rhs =
    match variable with 
    | Ref (GlobalVar _ as v) -> GlobalAssignment (v, rhs)
    | Ref (PredefinedVar _ as v) -> failwithf "convertAssignment: cannot set predefined variable"
    | Ref (LocalVar (_, mut, _) as v) ->
        mut := true
        LocalAssignment (v, rhs)
    | _ -> failwith "convertAssignment: Ref is expected as lhs"    

let processNaryClosedApplication variables body tail =
    failwith "not implemented"

let convertClosedApplication variables body tail =
    match List.tryLast variables with
    | Some v when isVariableDotted v ->
        processNaryClosedApplication variables body tail
    | _ when List.length variables = List.length tail ->
        FixLet (variables, tail, body)
    | _ -> failwith "convertClosedApplication: incorrect regular arity"
        

let convertApplication head tail =
    match head with
    | Function (variables, body) -> convertClosedApplication variables body tail


let rec convert env = function
    | List [Symbol "lambda"; names; body] ->
        let vars = getNames names
        let bodyConverted = convertSequence env body
        Function (vars, bodyConverted)
    | List [Symbol "if"; condition; consequence; alternative] ->
        Alternative (convert env condition,
                     convert env consequence,
                     convert env alternative)
    | Cons (Symbol "begin", exprs) -> convertSequence env exprs
    | List [Symbol "set!"; variable; rhs] ->
        let variable = convert env variable
        let rhs = convert env rhs
        convertAssignment variable rhs
    | List (head :: tail) ->
        let head = convert env head
        let tail = List.map (convert env) tail
        convertApplication head tail
    | Symbol name -> convertReference name env
   
and convertSequence env = function
    | List exprs -> List.map (convert env) exprs |> Sequence
    | _ -> failwith ""