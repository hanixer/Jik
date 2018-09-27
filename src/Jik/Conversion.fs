// <exp> ::= <const>
//        |  <prim>
//        |  <var>
//        |  (lambda (<var> ...) <exp>)
//        |  (if <exp> <exp> <exp>)
//        |  (set! <var> <exp>)
//        |  (<exp> <exp> ...)

// <const> ::= <int>
//          |  #f 

// Syntactic sugar:

// <exp> ::+ (let ((<var> <exp>) ...) <exp>)
//        |  (letrec ((<var> <exp>) ...) <exp>)
//        |  (begin <exp> ...)

module Conversion

open Base

// <exp> ::= <const>
//        |  

type Var =
    | GlobalVar of string
    | PredefinedVar of string
    | LocalVar of string * bool ref * bool

type Ref = Var

type Value =
    | Int of int
    | Bool of bool
    | Nil

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
    | Constant of Value
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
        | PredefinedVar n -> name = n
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
    | Function (variables, body) -> 
        convertClosedApplication variables body tail
    | Ref (PredefinedVar name) ->
        PredefinedApplication (name, tail)

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
    | Number n -> Int n |> Constant
    | SExpr.Bool b -> Bool b |> Constant
    | SExpr.Nil -> Constant Nil
    | e -> failwithf "nothing know about %A" e
   
and convertSequence env = function
    | List exprs -> List.map (convert env) exprs |> Sequence
    | _ -> failwith ""

let rec emit out = function
    | Constant (Int n) ->
        fprintf out "makeInt(%d)" n
    | Constant (Bool b) ->
        fprintf out "makeBool(%d)" <| if b then 1 else 0
    | Constant Nil ->
        fprintf out "makeNil()"
    | PredefinedApplication ("display", [arg]) ->
        fprintf out "printValue("
        emit out arg
        fprintfn out ");"
    | PredefinedApplication ("+", [arg1; arg2]) ->
        fprintf out "__plus("
        emit out arg1
        fprintf out ","
        emit out arg2
        fprintfn out ")"
    | Alternative (cond, conseq, altern) ->
        emitIf out cond conseq altern

and emitIf out cond conseq altern =
    fprintf out "!("
    emit out cond
    fprintf out ") ? ("
    emit out conseq
    fprintf out ") : "
    emit out altern
    // | e -> failwithf "emit: error: %A" e

let emitFull out program =
    fprintfn out "#include \"scheme.h\""
    fprintfn out ""
    fprintfn out "int main() {"
    emit out program
    fprintfn out "  return 0;"
    fprintfn out "}"


let compile filename str =
    predefinedEnv <- [
        PredefinedVar "display"
        PredefinedVar "+"
    ]
    use file = System.IO.File.CreateText filename
    let prog = stringToSExpr str |> convert []
    emitFull file prog
