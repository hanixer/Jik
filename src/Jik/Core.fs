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
    | MakeVector
    | IsVector
    | VectorLength
    | VectorRef
    | VectorSet
    | MakeClosure
    | ClosureRef
    | IsProcedure

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

type Program = (string * LambdaData) list * Expr

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
    "make-vector", MakeVector
    "vector?", IsVector
    "vector-length", VectorLength
    "vector-ref", VectorRef
    "vector-set!", VectorSet
    "procedure?", IsProcedure
]

let tryStringToPrimop s =
    List.tryFind (fst >> ((=) s)) stringPrimop
    |> Option.map snd

let isPrimop = tryStringToPrimop >> Option.isSome

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

let alphaRename (defs, expr) : Program =
    let newName (name, _) = name, freshLabel name

    let mapping =
        List.map newName defs
        |> Map.ofList

    let replaceInDef (name, (args, body)) =
        let name = 
            match Map.tryFind name mapping with
            | Some newName -> newName
            | _ -> name
        name, (args, List.map (alphaRename2 mapping) body)

    List.map replaceInDef defs, alphaRename2 mapping expr

let revealFunctions (prog : Program) : Program =
    let (defs, expr) = prog
    let functionNames = 
        List.map fst defs |> Set.ofList
    
    let rec handleExpr = function
        | If(exprc, exprt, exprf) -> If(handleExpr exprc, handleExpr exprt, handleExpr exprf)
        | Assign(var, expr) -> Assign(var, handleExpr expr)
        | Lambda(args, body) -> Lambda(args, List.map handleExpr body)
        | Begin(exprs) -> Begin(List.map handleExpr exprs)
        | App(func, args) -> App(handleExpr func, List.map handleExpr args)
        | PrimApp(op, args) -> PrimApp(op, List.map handleExpr args)
        | Ref(var) when Set.contains var functionNames -> FunctionRef var
        | e -> e

    let handleDef (name, (args, expr)) =
        name, (args, List.map handleExpr expr)

    List.map handleDef defs, handleExpr expr

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

    let handleDef (name, (args, expr)) =
        name, (args, List.map handleExpr2 expr)

    let defs, expr = prog

    List.map handleDef defs, handleExpr2 expr

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

let assignmentConvert expr =
    let alpha = alphaRename2 Map.empty expr
    let modified = findModifiedVars alpha
    let rec transform = function
        | Ref var ->
            if modified.Contains var then
                PrimApp(BoxRead, [Ref var])
            else
                Ref var
        | If(cond, conseq, altern) ->
            If(transform cond, transform conseq, transform altern)
        | Assign(var, rhs) -> 
            PrimApp(BoxWrite, [Ref var; transform rhs])
        | Lambda(args, body) -> 
            boxifyLambda args body
        | Begin exprs -> Begin(List.map transform exprs)
        | App(func, args) -> App(transform func, List.map transform args)
        | PrimApp(op, args) -> PrimApp(op, List.map transform args)
        | e -> e
    and boxifyLambda args body =
        let folder var body =
            if modified.Contains var then
                PrimApp(BoxCreate, [Ref var]) :: body
            else
                body
        let body = List.foldBack folder args (List.map transform body)
        Lambda(args, body)
    transform alpha

let stringToExpr = stringToSExpr >> sexprToExpr

let stringToProgram str : Program =
    let sexpr = stringToSExpr ("(" + str + ")")

    let parseDefs defs sexpr =
        match sexpr with
        | List (Symbol "define" :: List (Symbol name :: args) :: body) ->
            let args = symbolsToStrings args
            let body = List.map sexprToExpr body
            (name, (args, body)) :: defs
        | _ -> failwith "parseDefs: wrong format"

    match sexpr with
    | List sexprs when not (sexprs.IsEmpty) ->
        let sexprs = List.rev sexprs
        let expr = sexprToExpr (List.head sexprs)
        let defs = List.fold parseDefs [] (List.tail sexprs)
        (defs, expr)
    | _ -> failwith "stringToProgram: parsing failed"
