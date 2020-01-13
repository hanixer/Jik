module Core

open SExpr
open System.Collections.Generic

/// Core represents desugared form of input language.

type Prim =
    | BoxRead
    | BoxWrite
    | BoxCreate
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
        sprintf "%s%d" prefix count

let freshCodeLabel prefix = freshLabel (prefix + "/code")

let stringPrimop = [
    "+", Add
    "-", Sub
    "fx-", Sub
    "*", Mul
    "eq?", Eq
    "<", Lt
    "<=", Le
    ">", Gt
    ">=", Ge
    "not", Not
    "fixnum?", IsFixnum
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
    "number->char", NumberToChar
    "char->number", CharToNumber
    "fixnum->char", NumberToChar
    "char->fixnum", CharToNumber
    "char?", IsChar
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
    | List(Symbol "lambda" :: List args :: body) ->
        let argsString = symbolsToStrings args
        let env = argsString @ env
        let body = List.map (desugar2 env) body
        exprsToList (Symbol "lambda" :: exprsToList args :: body)
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
    | List(Symbol "letrec" :: List bindings :: body) ->
        let folder sexpr (bindings) =
            match sexpr with
            | List [Symbol name; initExpr] ->
                (name, initExpr) :: bindings
            | _ -> failwith "sexprToExpr: let: wrong bindings"

        let bindings = List.foldBack folder bindings []
        let names = List.map fst bindings
        let bindInitial name =
            S.Cons (Symbol name, S.Cons (Number 0, Nil))
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
    let rec loop processed = function
        | S.Cons(a, S.Cons(b, c)) ->
            loop (a :: processed) (S.Cons(b, c))
        | S.Cons(a, Nil) ->
            List.rev (a :: processed), false
        | S.Cons(a, b) ->
            List.rev (b :: a :: processed), true
        | Nil ->
            List.rev processed, false
        | _ -> failwith "parseArgs: parsing fails"

    match args with
    | Symbol arg -> [arg], true
    | _ ->
        let args, dotted = loop [] args
        symbolsToStrings args, dotted

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
        Lambda(args, dotted, convertList body)
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
            let lambda = exprsToList (Symbol "lambda" :: exprsToList args :: body)
            let sexpr = exprsToList [Symbol "set!"; Symbol name; lambda]
            name :: globals, sexpr :: sexprs
        | List [Symbol "define"; Symbol name; sexpr] ->
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

let convertGlobalRefs (prog : Program) : Program =
    let convertHelper propagate transform expr =
        match expr with
        | Expr.Ref var when List.contains var prog.Globals ->
            PrimApp(GlobalRef, [Ref var])
        | Expr.Assign(var, rhs) when List.contains var prog.Globals ->
            PrimApp(GlobalSet, [Ref var; rhs])
        | _ -> propagate expr

    let convertExpr expr = transform convertHelper expr

    { prog with Main = List.map convertExpr prog.Main }

let trySubstitute v mapping =
    match Map.tryFind v mapping with
    | Some(w) -> w
    | _ -> v

let extendMapping vars mapping =
    let newVars = List.map freshLabel vars
    let folder mapping (oldVar, newVar) = Map.add oldVar newVar mapping
    newVars, List.fold folder mapping (List.zip vars newVars)

let rec alphaRename2 mapping =
    function
    | Ref v -> trySubstitute v mapping |> Ref
    | If(cond, thenExpr, elseExpr) ->
        If
            (alphaRename2 mapping cond, alphaRename2 mapping thenExpr,
             alphaRename2 mapping elseExpr)
    | Assign(var, rhs) ->
        Assign(trySubstitute var mapping, alphaRename2 mapping rhs)
    | Lambda(args, dotted, body) ->
        let newVars, mapping = extendMapping args mapping
        Lambda(newVars, dotted, List.map (alphaRename2 mapping) body)
    | Begin exprs -> Begin(List.map (alphaRename2 mapping) exprs)
    | App(func, argsExprs) ->
        App(alphaRename2 mapping func, List.map (alphaRename2 mapping) argsExprs)
    | PrimApp(op, args) ->
        PrimApp(op, List.map (alphaRename2 mapping) args)
    | e -> e

let alphaRename (prog : Program) : Program =
    let newGlobals, mapping = extendMapping prog.Globals Map.empty
    let rename = alphaRename2 mapping
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
    let rec convertRefs (modified : Set<string>) expr =
        let conv = convertRefs modified
        match expr with
        | Ref var ->
            if modified.Contains var then
                PrimApp(BoxRead, [Ref var])
            else
                Ref var
        | If(cond, conseq, altern) ->
            If(conv cond, conv conseq, conv altern)
        | Assign(var, rhs) ->
            PrimApp(BoxWrite, [Ref var; conv rhs])
        | Lambda(args, dotted, body) ->
            let body = convertLambda modified args body
            Lambda(args, dotted, body)
        | Begin exprs -> Begin(List.map conv exprs)
        | App(func, args) -> App(conv func, List.map conv args)
        | PrimApp(op, args) -> PrimApp(op, List.map conv args)
        | e -> e

    and convertLambda modified args body =
        let body = List.map (convertRefs modified) body
        List.foldBack (fun arg body ->
            if Set.contains arg modified then
                PrimApp(BoxCreate, [Ref arg]) :: body
            else
                body)
            args body

    let convertFunc (name, (args, body)) =
        let modified = findModifiedVars (Begin body)
        let body = convertLambda modified args body
        name, (args, body)

    let modified = List.map findModifiedVars prog.Main |> Set.unionMany
    { prog with Main = List.map (convertRefs modified) prog.Main }

let stringToExpr = stringToSExpr >> sexprToExpr
