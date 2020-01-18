module Core

open SExpr
open System.Collections.Generic

/// Core represents desugared form of input language.

type Prim =
    | Add
    | Sub
    | Mul
    | Eq
    | Lt
    | Le
    | Gt
    | Ge
    | Not
    | IsFixnum
    | IsBoolean
    | Cons
    | IsPair
    | IsNull
    | Car
    | Cdr
    | SetCar
    | SetCdr
    | MakeVector
    | IsVector
    | VectorLength
    | VectorRef
    | VectorSet
    | MakeString
    | IsString
    | StringLength
    | StringRef
    | StringSet
    | MakeClosure
    | ClosureRef
    | IsProcedure
    | GlobalRef
    | GlobalSet
    | IsZero
    | NumberToChar
    | CharToNumber
    | IsChar
    | Apply

type Expr =
    | Int of int
    | Char of char
    | Bool of bool
    | String of string
    | EmptyList
    | Ref of string
    | If of Expr * Expr * Expr
    | Assign of string * Expr
    | Lambda of LambdaData
    | Begin of Expr list
    | App of Expr * Expr list
    | PrimApp of Prim * Expr list
    | ForeignCall of foreignName : string * args : Expr list

and LambdaData = string list * bool * Expr list

type Program =
    { Main : Expr list
      Globals : string list }

type S = SExpr

let freshLabel =
    let mutable dict  = Dictionary<string, int>()
    fun prefix ->
        if dict.ContainsKey prefix |> not then
            dict.Add(prefix, 0)
        let count = dict.Item prefix
        dict.Item prefix <- count + 1
        if count = 0 then
            prefix
        else
            sprintf "%s%d" prefix count

let freshCodeLabel prefix = freshLabel (prefix + "/code")

let stringPrimop = [
    "+", Add
    "-", Sub
    "fx+", Add
    "fx-", Sub
    "*", Mul
    "fx*", Mul
    "eq?", Eq
    "<", Lt
    "<=", Le
    ">", Gt
    ">=", Ge
    "not", Not
    "fixnum?", IsFixnum
    "number?", IsFixnum
    "boolean?", IsBoolean
    "cons", Cons
    "pair?", IsPair
    "null?", IsNull
    "car", Car
    "cdr", Cdr
    "set-car!", SetCar
    "set-cdr!", SetCdr
    "make-vector", MakeVector
    "vector?", IsVector
    "vector-length", VectorLength
    "vector-ref", VectorRef
    "vector-set!", VectorSet
    "make-string", MakeString
    "string?", IsString
    "string-length", StringLength
    "string-ref", StringRef
    "string-set!", StringSet
    "procedure?", IsProcedure
    "zero?", IsZero
    "fxzero?", IsZero
    "number->char", NumberToChar
    "char->number", CharToNumber
    "fixnum->char", NumberToChar
    "char->fixnum", CharToNumber
    "char?", IsChar
    "apply", Apply
]

let tryStringToPrimop s =
    List.tryFind (fst >> ((=) s)) stringPrimop
    |> Option.map snd

let isPrimop = tryStringToPrimop >> Option.isSome

let wrapInLet var value body =
    exprsToList [S.Symbol "let"
                 exprsToList [exprsToList [var; value]]
                 body]

let undefinedExpr = S.Bool false

let special = [
    "lambda"
    "let"
    "let*"
    "letrec"
    "letrec*"
    "and"
    "or"
    "cond"
    "else"
    "when"
    "unless"
]

let isUnused s env = List.contains s env |> not

let rec desugar2 env sexpr =
    match sexpr with
    | List (Symbol s :: rest) when (List.contains s special) && (List.contains s env |> not) ->
        expand env sexpr
    | List sexprs ->
        exprsToList (List.map (desugar2 env) sexprs)
    | _ -> sexpr

and expand env sexpr =
    let desugar = desugar2 env
    match sexpr with
    | List(Symbol "lambda" :: Symbol arg :: body) ->
        let env = arg :: env
        let body = List.map (desugar2 env) body
        exprsToList (Symbol "lambda" :: Symbol arg :: body)
    | List(Symbol "lambda" :: List args :: body) ->
        let argsString = symbolsToStrings args
        let env = argsString @ env
        let body = List.map (desugar2 env) body
        exprsToList (Symbol "lambda" :: exprsToList args :: body)
    | List(Symbol "lambda" :: ListImproper args :: body) ->
        let argsString = symbolsToStrings args
        let env = argsString @ env
        let body = List.map (desugar2 env) body
        let argsRevert = exprsToDottedList args
        SExpr.Cons(Symbol "lambda", SExpr.Cons(argsRevert, exprsToList body))
    | List(Symbol "let" :: List bindings :: body) ->
        let folder sexpr (names, initExprs) =
            match sexpr with
            | List [Symbol name; initExpr] ->
                name :: names, initExpr :: initExprs
            | _ -> failwith "sexprToExpr: let: wrong bindings"

        let names, initExprs = List.foldBack folder bindings ([], [])
        let args = List.map Symbol names
        let lam = exprsToList (Symbol "lambda" :: exprsToList args :: body)
        exprsToList (lam :: initExprs)
        |> desugar2 env
    | List(Symbol "let" :: Symbol name :: List bindings :: body) ->
        let bindings = List.map (function | List [Symbol name; sexpr] -> Symbol name, sexpr | _ -> failwith "wrong binding") bindings
        let names, initExprs = List.unzip bindings
        let lambda = exprsToList (Symbol "lambda" :: exprsToList names :: body)
        let call = exprsToList (Symbol name :: initExprs)
        let binding = exprsToList [exprsToList [Symbol name; lambda]]
        let letrec = exprsToList [Symbol "letrec"; binding; call]
        desugar letrec
    | List(Symbol "letrec" :: List bindings :: body) ->
        let folder sexpr (bindings) =
            match sexpr with
            | List [Symbol name; initExpr] ->
                (name, initExpr) :: bindings
            | _ -> failwith "sexprToExpr: let: wrong bindings"

        let bindings = List.foldBack folder bindings []
        let names = List.map fst bindings
        let bindInitial name = exprsToList [Symbol name; Number 0]
        let letBindings =
            names
            |> List.map bindInitial
            |> exprsToList
        let body = List.foldBack (fun (name, expr) acc ->
            exprsToList [Symbol "set!"; Symbol name; expr] :: acc) bindings body
        exprsToList (Symbol "let" :: letBindings :: body)
        |> desugar
    | List [S.Symbol "and"] -> S.Bool true
    | List [S.Symbol "and"; a] ->
        let a = desugar2 env a
        exprsToList [S.Symbol "if"; a; a; S.Bool false]
    | List (S.Symbol "and" :: a :: rest) ->
        let a = desugar a
        let rest = exprsToList (S.Symbol "and" :: rest)
        exprsToList [S.Symbol "if"; a; desugar rest; S.Bool false]
    | List [S.Symbol "or"] -> S.Bool false
    | List [S.Symbol "or"; a] ->
        desugar a
    | List (S.Symbol "or" :: a :: rest) ->
        let a = desugar a
        let t = S.Symbol (freshLabel "t")
        let rest =
            exprsToList (S.Symbol "or" :: rest)
            |> desugar
        exprsToList [S.Symbol "if"; t; t; rest]
        |> wrapInLet t a
        |> expand env
    | List (S.Symbol "when" :: condition :: body) ->
        let condition = desugar condition
        let body = exprsToList (S.Symbol "begin" :: List.map desugar body)
        exprsToList [S.Symbol "if"; condition; body; undefinedExpr]
    | List (S.Symbol "unless" :: condition :: body) ->
        let condition = desugar condition
        let body = exprsToList (S.Symbol "begin" :: List.map desugar body)
        exprsToList [S.Symbol "if"; condition; undefinedExpr; body]
    | List [S.Symbol "cond"] ->
        undefinedExpr
    | List [S.Symbol "cond"; List [S.Symbol "else"; expr]] when isUnused "else" env ->
        desugar expr
    | List (S.Symbol "cond" :: List [clause] :: rest) ->
        let clause = desugar clause
        let t = S.Symbol (freshLabel "t")
        let rest = (exprsToList (S.Symbol "cond" :: rest))
        wrapInLet t clause (exprsToList [S.Symbol "if"; t; t; rest])
        |> expand env
    | List (S.Symbol "cond" :: List [condition; S.Symbol "=>"; conseq] :: rest) when isUnused "=>" env ->
        let condition = desugar condition
        let conseq = desugar conseq
        let t = S.Symbol (freshLabel "t")
        let t2 = S.Symbol (freshLabel "t2")
        let rest = desugar (exprsToList (S.Symbol "cond" :: rest))
        wrapInLet t2 conseq (exprsToList [S.Symbol "if"; t; exprsToList [t2; t]; rest])
        |> expand env
        |> wrapInLet t condition
        |> expand env
    | List (S.Symbol "cond" :: List (condition :: conseq) :: rest) ->
        let condition = desugar condition
        let conseq = exprsToList (S.Symbol "begin" :: List.map desugar conseq)
        let rest = desugar (exprsToList (S.Symbol "cond" :: rest))
        exprsToList [S.Symbol "if"; condition; conseq; rest]
    | _ ->
        printfn "Oh no! %A" sexpr
        sexpr

let parseArgs args =
    match args with
    | List args -> args, false
    | ListImproper (args) -> args, true
    | Symbol arg -> [Symbol arg], true
    | _ -> failwithf "wrong argument SExpr for lambda form:\n%A" args

let rec sexprToExpr sexpr =
    let convertList = List.map sexprToExpr
    match sexpr with
    | SExpr.Char c -> Char c
    | SExpr.Number n -> Int n
    | SExpr.Bool n -> Bool n
    | SExpr.Symbol name -> Ref name
    | SExpr.String string -> String string
    | List [SExpr.Symbol "if"; cond; conseq; altern] ->
        If(sexprToExpr cond, sexprToExpr conseq, sexprToExpr altern)
    | List(Symbol "begin" :: e :: exprs) ->
        let es = convertList exprs
        Begin(sexprToExpr e :: es)
    | List [Symbol "set!"; Symbol name; rhs] -> Assign(name, sexprToExpr rhs)
    | List(Symbol "lambda" :: args :: body) ->
        let args, dotted = parseArgs args
        let strings = symbolsToStrings args
        Lambda(strings, dotted, convertList body)
    | List [Symbol "quote"; List []] -> EmptyList
    | List [Symbol "quote"; form] ->
        failwith "sexprToExpr: quote is not supported"
    | List (Symbol "foreign-call" :: SExpr.String foreignName :: tail) ->
        let args = convertList tail
        ForeignCall(foreignName, args)
    | List(Symbol op :: tail) when isPrimop op ->
        let op = tryStringToPrimop op
        PrimApp(Option.get op, convertList tail)
    | List(head :: tail) -> App(sexprToExpr head, convertList tail)
    | e -> failwith <| sexprToString e

let stringToProgram str : Program =
    let sexpr = stringToSExpr ("(" + str + ")")

    let parseSExpr (globals, sexprs) sexpr =
        match sexpr with
        | List (Symbol "define" :: List (Symbol name :: args) :: body) ->
            printfn "global: %s" name
            let lambda = exprsToList (Symbol "lambda" :: exprsToList args :: body)
            let sexpr = exprsToList [Symbol "set!"; Symbol name; lambda]
            name :: globals, sexpr :: sexprs
        | List [Symbol "define"; Symbol name; sexpr] ->
            printfn "global: %s" name
            let sexpr = exprsToList [Symbol "set!"; Symbol name; sexpr]
            name :: globals, sexpr :: sexprs
        | sexpr ->
            globals, sexpr :: sexprs

    let desugared = desugar2 [] sexpr

    match desugared with
    | List sexprs when not (sexprs.IsEmpty) ->
        let globals, sexprs = List.fold parseSExpr ([], []) sexprs
        let exprList = List.map sexprToExpr sexprs
        { Main = exprList |> List.rev
          Globals = List.rev globals }
    | _ -> failwith "stringToProgram: parsing failed"

let rec transform f expr =
    let rec propagate expr =
        match expr with
        | Expr.If(cond, conseq, altern) -> Expr.If(transform cond, transform conseq, transform altern)
        | Expr.Assign(var, rhs) -> Expr.Assign(var, transform rhs)
        | Expr.Lambda(args, dotted, body) -> Expr.Lambda(args, dotted, List.map transform body)
        | Expr.Begin(exprs) -> Expr.Begin(List.map transform exprs)
        | Expr.App(func, args) -> Expr.App(transform func, List.map transform args)
        | Expr.PrimApp(op, args) -> Expr.PrimApp(op, List.map transform args)
        | e -> e

    and transform expr =
        fpt expr

    and fpt = f propagate transform

    transform expr

//   (define (convert-char char)
//     (case char
//       ((#\_)   '(#\_ #\_))
//       ((#\?)   '(#\p))
//       ((#\!)   '(#\i))
//       ((#\<)   '(#\l))
//       ((#\>)   '(#\g))
//       ((#\=)   '(#\e))
//       ((#\- #\/ #\* #\:)   '())
//       (else (list char))))

let convertSchemeIdentifToAsm name =
    let convertChar = function
        | '?' -> "Qmark"
        | '!' -> "Bang"
        | '<' -> "Less"
        | '>' -> "Greater"
        | '=' -> "Eq"
        | '-' -> "Minus"
        | '+' -> "Plus"
        | '*' -> "Times"
        | '/' -> "Divide"
        | '.' -> "Dot"
        | '$' -> "Dollar"
        | c -> sprintf "%c" c

    Seq.map convertChar name
    |> System.String.Concat

let convertGlobalRefs (prog : Program) : Program =
    let newNames = List.map convertSchemeIdentifToAsm prog.Globals
    let env = List.zip prog.Globals newNames |> Map.ofSeq

    let convertHelper propagate transform expr =
        match expr with
        | Expr.Ref var when List.contains var prog.Globals ->
            let newName = Map.find var env
            PrimApp(GlobalRef, [Ref newName])
        | Expr.Assign(var, rhs) when List.contains var prog.Globals ->
            let newName = Map.find var env
            let newRhs = transform rhs
            PrimApp(GlobalSet, [Ref newName; newRhs])
        | _ -> propagate expr

    let convertExpr expr = transform convertHelper expr

    { prog with Main = List.map convertExpr prog.Main
                Globals = newNames }

let trySubstitute v mapping =
    match Map.tryFind v mapping with
    | Some(w) -> w
    | _ -> v

let extendMapping vars mapping =
    let newVars = List.map freshLabel vars
    let folder mapping (oldVar, newVar) = Map.add oldVar newVar mapping
    newVars, List.fold folder mapping (List.zip vars newVars)

// TODO: change this to use transform instead
let rec alphaRenameImpl mapping =
    function
    | Ref v -> trySubstitute v mapping |> Ref
    | If(cond, thenExpr, elseExpr) ->
        If(alphaRenameImpl mapping cond, alphaRenameImpl mapping thenExpr,
             alphaRenameImpl mapping elseExpr)
    | Assign(var, rhs) ->
        Assign(trySubstitute var mapping, alphaRenameImpl mapping rhs)
    | Lambda(args, dotted, body) ->
        let newVars, mapping = extendMapping args mapping
        Lambda(newVars, dotted, List.map (alphaRenameImpl mapping) body)
    | Begin exprs -> Begin(List.map (alphaRenameImpl mapping) exprs)
    | App(func, argsExprs) ->
        App(alphaRenameImpl mapping func, List.map (alphaRenameImpl mapping) argsExprs)
    | PrimApp(op, args) ->
        PrimApp(op, List.map (alphaRenameImpl mapping) args)
    | ForeignCall(foreignName, args) ->
        ForeignCall(foreignName, List.map (alphaRenameImpl mapping) args)
    | e -> e

let alphaRename (prog : Program) : Program =
    let newGlobals, mapping = extendMapping prog.Globals Map.empty
    let rename = alphaRenameImpl mapping
    { prog with Main = List.map rename prog.Main
                Globals = newGlobals }

let rec replaceVars mapping expr =
    let transf = replaceVars mapping
    let replace var =
        match Map.tryFind var mapping with
        | Some var2 -> var2
        | _ -> var
    match expr with
    | Expr.Ref var -> replace var |> Expr.Ref
    | Expr.If(cond, conseq, altern) -> Expr.If(transf cond, transf conseq, transf altern)
    | Expr.Assign(var, rhs) -> Expr.Assign(replace var, transf rhs)
    | Expr.Lambda(args, dotted, body) -> Expr.Lambda(args, dotted, List.map transf body)
    | Expr.Begin(exprs) -> Expr.Begin(List.map transf exprs)
    | Expr.App(func, args) -> Expr.App(transf func, List.map transf args)
    | Expr.PrimApp(op, args) -> Expr.PrimApp(op, List.map transf args)
    | Expr.ForeignCall(foreignName, args) -> Expr.ForeignCall(foreignName, List.map transf args)
    | e -> e

let rec fixArithmeticPrims (prog : Program) : Program =
    let ops = [Add; Mul;]

    let fold op acc curr =
        PrimApp(op, [acc; curr])

    let rec handleExpr propagate transform expr =
        match expr with
        | Expr.PrimApp(op, args) when (List.contains op ops) && (args.Length > 1) ->
            let args = List.map transform args
            List.fold (fold op) args.Head args.Tail
        | e -> propagate e

    let handleExpr2 = transform handleExpr

    { prog with Main = List.map handleExpr2 prog.Main }

let rec findModifiedVars expr =
    let find = findModifiedVars
    let findMany = List.map find >> Set.unionMany
    match expr with
    | Ref _ -> Set.empty
    | If (cond, thenc, elsec) ->
        findMany [cond; thenc; elsec]
    | Assign (v, rhs) ->
        Set.add v (find rhs)
    | App (head, tail) ->
        findMany (head :: tail)
    | Lambda (args, dotted, body) ->
        findMany body
    | Begin exprs ->
        findMany exprs
    | PrimApp(op, tail) ->
        findMany tail
    | _ -> Set.empty

/// Convert assignments to box writes.
let assignmentConvert (prog : Program) : Program =

    let newNamesEnv args modified =
        List.filter (fun arg -> Set.contains arg modified) args
        |> List.map (fun arg -> arg, freshLabel "v")
        |> Map.ofList

    let rec convertRefs (modified : Set<string>) expr =
        let conv = convertRefs modified
        match expr with
        | Ref var ->
            if modified.Contains var then
                PrimApp(VectorRef, [Ref var; Int 0])
            else
                Ref var
        | If(cond, conseq, altern) ->
            If(conv cond, conv conseq, conv altern)
        | Assign(var, rhs) ->
            PrimApp(VectorSet, [Ref var; Int 0; conv rhs])
        | Lambda(args, dotted, body) ->
            let (newArgs, newBody) = convertLambda modified args body
            Lambda(newArgs, dotted, newBody)
        | Begin exprs -> Begin(List.map conv exprs)
        | App(func, args) -> App(conv func, List.map conv args)
        | PrimApp(op, args) -> PrimApp(op, List.map conv args)
        | ForeignCall(name, args) -> ForeignCall(name, List.map conv args)
        | e -> e

    /// Change each modified arg to new name,
    /// then introduce variable with original name,
    /// which refers to vector.
    and convertLambda modified args body =
        let env = newNamesEnv args modified
        let body = List.map (convertRefs modified) body
        List.foldBack (fun arg (args, body) ->
            match Map.tryFind arg env with
            | Some(newArg) ->
                let set = PrimApp(VectorSet, [Ref arg; Int 0; Ref newArg])
                let lambda = Lambda([arg], false, set :: body)
                let make = PrimApp(MakeVector, [Int 1])
                newArg :: args, [App(lambda, [make])]
            | _ ->
                arg :: args, body)
            args ([], body)

    let convertFunc (name, (args, body)) =
        let modified = findModifiedVars (Begin body)
        let (newArgs, newBody) = convertLambda modified args body
        name, (newArgs, newBody)

    let modified = List.map findModifiedVars prog.Main |> Set.unionMany
    { prog with Main = List.map (convertRefs modified) prog.Main }

let stringToExpr = stringToSExpr >> sexprToExpr

let allCoreTransformations =
    fixArithmeticPrims
    >> convertGlobalRefs
    >> alphaRename
    >> assignmentConvert
