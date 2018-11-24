module Core

open SExpr
open System.Collections.Generic

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
    | Cons
    | IsPair
    | Car
    | Cdr
    | MakeVector
    | IsVector
    | VectorLength
    | VectorRef
    | VectorSet
    | MakeClosure
    | ClosureRef
    | IsProcedure
    | GlobalRef
    | GlobalSet
    | IsZero

type Expr = 
    | Int of int
    | Bool of bool
    | Ref of string
    | FunctionRef of string
    | If of Expr * Expr * Expr
    | Assign of string * Expr
    | Lambda of LambdaData
    | Begin of Expr list
    | App of Expr * Expr list
    | PrimApp of Prim * Expr list

and LambdaData = string list * Expr list

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
    "*", Mul
    "eq?", Eq
    "<", Lt
    "<=", Le
    ">", Gt
    ">=", Ge
    "not", Not
    "cons", Cons
    "pair?", IsPair
    "car", Car
    "cdr", Cdr
    "make-vector", MakeVector
    "vector?", IsVector
    "vector-length", VectorLength
    "vector-ref", VectorRef
    "vector-set!", VectorSet
    "procedure?", IsProcedure
    "zero?", IsZero
]

let tryStringToPrimop s =
    List.tryFind (fst >> ((=) s)) stringPrimop
    |> Option.map snd

let isPrimop = tryStringToPrimop >> Option.isSome

let rec desugar sexpr =
    match sexpr with
    | List [S.Symbol "and"] -> S.Bool true
    | List [S.Symbol "and"; a] ->
        let a = desugar a
        exprsToList [S.Symbol "if"; a; a; S.Bool false]
    | List (S.Symbol "and" :: a :: rest) ->
        let a = desugar a
        exprsToList [S.Symbol "if"; a; desugar (exprsToList (S.Symbol "and" :: rest)); S.Bool false]
    | List [S.Symbol "or"] -> S.Bool false
    | List [S.Symbol "or"; a] ->
        desugar a
    | List (S.Symbol "or" :: a :: rest) ->
        let a = desugar a
        exprsToList [S.Symbol "if"; a; a; desugar (exprsToList (S.Symbol "or" :: rest))]
    | List sexprs -> exprsToList (List.map desugar sexprs)
    | e -> e

let rec sexprToExpr sexpr = 
    let convertList = List.map sexprToExpr
    match sexpr with
    | SExpr.Number n -> Int n
    | SExpr.Bool n -> Bool n
    | SExpr.Symbol name -> Ref name
    | List [SExpr.Symbol "if"; cond; conseq; altern] -> 
        If(sexprToExpr cond, sexprToExpr conseq, sexprToExpr altern)
    | List(Symbol "begin" :: e :: exprs) -> 
        let es = convertList exprs
        Begin(sexprToExpr e :: es)
    | List [Symbol "set!"; Symbol name; rhs] -> Assign(name, sexprToExpr rhs)
    | List(Symbol "lambda" :: List args :: body) -> 
        Lambda(symbolsToStrings args, convertList body)
    | List [Symbol "quote"; form] -> 
        failwith "sexprToExpr: quote is not supported"
    | List(Symbol "let" :: List bindings :: body) -> 
        let folder sexpr (names, initExprs) = 
            match sexpr with
            | List [Symbol name; initExpr] -> 
                name :: names, initExpr :: initExprs
            | _ -> failwith "sexprToExpr: let: wrong bindings"
        
        let names, initExprs = List.foldBack folder bindings ([], [])
        let lam = Lambda(names, convertList body)
        let argExprs = convertList initExprs
        App(lam, argExprs)
    | List(Symbol "letrec" :: List bindings :: body) -> 
        let bindings = transformLetrecBindings bindings
        let fnames = List.map fst bindings
        let bindInitial fname = 
            Cons (Symbol fname, Cons (Number 0, Nil))
        let letBindings = 
            fnames
            |> List.map bindInitial
            |> exprsToList
        let foldSets (name, (args, lamBody)) next =
            let args = List.map Symbol args |> exprsToList
            let lambda = exprsToList [Symbol "lambda"; args; lamBody]
            let set = exprsToList [Symbol "set!"; Symbol name; lambda]
            set :: next
        let body =
            List.foldBack foldSets bindings body
        exprsToList (Symbol "let" :: letBindings :: body)
        |> sexprToExpr
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

    match desugar sexpr with
    | List sexprs when not (sexprs.IsEmpty) ->
        let globals, sexprs = List.fold parseSExpr ([], []) sexprs
        { Main = List.map sexprToExpr sexprs |> List.rev
          Globals = List.rev globals }
    | _ -> failwith "stringToProgram: parsing failed"

let rec transform f expr =
    let rec propagate expr = 
        match expr with
        | Expr.If(cond, conseq, altern) -> Expr.If(transform cond, transform conseq, transform altern)
        | Expr.Assign(var, rhs) -> Expr.Assign(var, transform rhs)
        | Expr.Lambda(args, body) -> Expr.Lambda(args, List.map transform body)
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
    | Lambda(args, body) -> 
        let newVars, mapping = extendMapping args mapping
        Lambda(newVars, List.map (alphaRename2 mapping) body)
    | Begin exprs -> Begin(List.map (alphaRename2 mapping) exprs)
    | App(func, argsExprs) -> 
        App(alphaRename2 mapping func, List.map (alphaRename2 mapping) argsExprs)
    | PrimApp(op, args) ->
        PrimApp(op, List.map (alphaRename2 mapping) args)
    | e -> e

let alphaRename (prog : Program) : Program =
    { prog with Main = List.map (alphaRename2 Map.empty) prog.Main }

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
    | Expr.Lambda(args, body) -> Expr.Lambda(args, List.map transf body)
    | Expr.Begin(exprs) -> Expr.Begin(List.map transf exprs)
    | Expr.App(func, args) -> Expr.App(transf func, List.map transf args)
    | Expr.PrimApp(op, args) -> Expr.PrimApp(op, List.map transf args)
    | e -> e

let revealFunctions (prog : Program) : Program =
    // let (defs, expr) = prog
    // let functionNames = 
    //     List.map fst defs |> Set.ofList
    
    // let rec handleExpr = function
    //     | If(exprc, exprt, exprf) -> If(handleExpr exprc, handleExpr exprt, handleExpr exprf)
    //     | Assign(var, expr) -> Assign(var, handleExpr expr)
    //     | Lambda(args, body) -> Lambda(args, List.map handleExpr body)
    //     | Begin(exprs) -> Begin(List.map handleExpr exprs)
    //     | App(func, args) -> App(handleExpr func, List.map handleExpr args)
    //     | PrimApp(op, args) -> PrimApp(op, List.map handleExpr args)
    //     | Ref(var) when Set.contains var functionNames -> FunctionRef var
    //     | e -> e

    // let handleDef (name, (args, expr)) =
    //     name, (args, List.map handleExpr expr)

    // List.map handleDef defs, handleExpr expr
    prog 

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
    | Lambda (args, body) ->
        findMany body
    | Begin exprs ->
        findMany exprs
    | _ -> Set.empty

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
        | Lambda(args, body) -> 
            let body = convertLambda modified args body
            Lambda(args, body)
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
