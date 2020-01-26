module Core

open SExpr
open System.Collections.Generic
open Common
open Desugar
open Primitive
open Library

/// Core represents desugared form of input language.

type Expr =
    | Int of int
    | Char of char
    | Bool of bool
    | String of string
    | EmptyList
    | Symbol of string
    | Quote of Expr
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
      Globals : string list
      ConstantsNames : string list
      Strings : (string * string) list }

type S = SExpr

let tryStringToPrimop s =
    List.tryFind (fst >> ((=) s)) stringPrimop
    |> Option.map snd

let isPrimop = tryStringToPrimop >> Option.isSome

let parseArgs args =
    match args with
    | List args -> args, false
    | ListImproper (args) -> args, true
    | S.Symbol arg -> [S.Symbol arg], true
    | _ -> failwithf "wrong argument SExpr for lambda form:\n%A" args

let isSimple = function
    | S.Char _
    | S.Number _
    | S.Bool _
    | S.String _ -> true
    | _ -> false

let convertSimple = function
    | S.Char c -> Char c
    | S.Number n -> Int n
    | S.Bool n -> Bool n
    | S.String string -> String string
    | S.Nil -> EmptyList
    | _ -> failwith "simple sexpr expected"

let rec quotedFormToExpr form =
    match form with
    | _ when isSimple form -> convertSimple form
    | S.Nil -> EmptyList
    | S.Symbol s -> Symbol s
    | S.Cons(x, y) ->
        let xx = quotedFormToExpr x
        let yy = quotedFormToExpr y
        PrimApp(Cons, [xx; yy])
    | _ -> failwith ""

let rec sexprToExpr sexpr =
    let convertList = List.map sexprToExpr
    match sexpr with
    | _ when isSimple sexpr -> convertSimple sexpr
    | S.Symbol "nil" -> EmptyList
    | S.Symbol name -> Ref name
    | List [S.Symbol "if"; cond; conseq; altern] ->
        If(sexprToExpr cond, sexprToExpr conseq, sexprToExpr altern)
    | List (S.Symbol "if" :: _) ->
        failwithf "wrong 'if' form:\n%A" (sexprToString sexpr)
    | List(S.Symbol "begin" :: e :: exprs) ->
        let es = convertList exprs
        Begin(sexprToExpr e :: es)
    | List [S.Symbol "set!"; S.Symbol name; rhs] -> Assign(name, sexprToExpr rhs)
    | List(S.Symbol "lambda" :: args :: body) ->
        if List.isEmpty body then
            failwithf "lambda: body should not be empty\n%s\n" (sexprToString sexpr)

        let args, dotted = parseArgs args
        let strings = symbolsToStrings args
        Lambda(strings, dotted, convertList body)
    | List [S.Symbol "quote"; List []] -> EmptyList
    | List [S.Symbol "quote"; form] ->
        Quote(quotedFormToExpr form)
    | List (S.Symbol "foreign-call" :: S.String foreignName :: tail) ->
        let args = convertList tail
        ForeignCall(foreignName, args)
    | List(S.Symbol op :: tail) when isPrimop op ->
        let op = tryStringToPrimop op
        PrimApp(Option.get op, convertList tail)
    | List(head :: tail) -> App(sexprToExpr head, convertList tail)
    | e -> failwith <| sexprToString e

let stringToProgram str : Program =
    let sexpr = stringToSExpr ("(\n" + str + "\n)")

    let convertDefinitions (globals, sexprs) sexpr =
        match sexpr with
        | List (S.Symbol "define" :: List (S.Symbol name :: args) :: body) ->
            let lambda = exprsToList (S.Symbol "lambda" :: exprsToList args :: body)
            let sexpr = exprsToList [S.Symbol "set!"; S.Symbol name; lambda]
            name :: globals, sexpr :: sexprs
        | List [S.Symbol "define"; S.Symbol name; sexpr] ->
            let sexpr = exprsToList [S.Symbol "set!"; S.Symbol name; sexpr]
            name :: globals, sexpr :: sexprs
        | sexpr ->
            globals, sexpr :: sexprs

    let desugared = desugar sexpr

    match desugared with
    | sexprs when not (sexprs.IsEmpty) ->
        let globals, sexprs = List.fold convertDefinitions ([], []) sexprs
        let exprList = List.map sexprToExpr sexprs
        { Main = exprList |> List.rev
          Globals = List.rev globals
          Strings = []
          ConstantsNames = [] }
    | _ -> failwith "stringToProgram: parsing failed"

let rec transform f expr =
    let rec propagate expr =
        match expr with
        | If(cond, conseq, altern) -> If(transform cond, transform conseq, transform altern)
        | Assign(var, rhs) -> Assign(var, transform rhs)
        | Lambda(args, dotted, body) -> Lambda(args, dotted, List.map transform body)
        | Begin(exprs) -> Begin(List.map transform exprs)
        | App(func, args) -> App(transform func, List.map transform args)
        | PrimApp(op, args) -> PrimApp(op, List.map transform args)
        | ForeignCall(name, args) -> ForeignCall(name, List.map transform args)
        | e -> e

    and transform expr =
        fpt expr

    and fpt = f propagate transform

    transform expr

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
        | '$' -> "Dollar"
        | '%' -> "Percent"
        | c -> sprintf "%c" c

    Seq.map convertChar name
    |> System.String.Concat

let convertGlobalRefs (prog : Program) : Program =
    let globals = System.Collections.Generic.HashSet<string>()

    let addGlobal var =
        let newVar = convertSchemeIdentifToAsm var
        globals.Add(newVar) |> ignore
        newVar

    let rec convert env expr =
        match expr with
        | Ref(var) when List.contains var env -> Ref(var)
        | Ref(var) ->
            let newVar = addGlobal var
            PrimApp(GlobalRef, [Ref newVar])
        | Assign(var, rhs) ->
            let rhs = convert env rhs
            if List.contains var env then
                Assign(var, rhs)
            else
                let newVar = addGlobal var
                PrimApp(GlobalSet, [Ref newVar; rhs])
        | Lambda(args, dotted, body) ->
            let env = args @ env
            let body = List.map (convert env) body
            Lambda(args, dotted, body)
        | App(func, args) ->
            let func = convert env func
            let args = List.map (convert env) args
            App(func, args)
        | If(cond, conseq, altern) ->
            let cond = convert env cond
            let conseq = convert env conseq
            let altern = convert env altern
            If(cond, conseq, altern)
        | PrimApp(StringInit, _) -> expr
        | PrimApp(p, args) ->
            let args = List.map (convert env) args
            PrimApp(p, args)
        | Begin(exprs) ->
            let exprs = List.map (convert env) exprs
            Begin(exprs)
        | ForeignCall(foreignName, args) ->
            let args = List.map (convert env) args
            ForeignCall(foreignName, args)
        | _ -> expr

    let main = List.map (convert []) prog.Main

    // Remove constant references from globals list.
    globals.RemoveWhere(fun g -> Seq.contains g prog.ConstantsNames) |> ignore

    { prog with Main = main
                Globals = Seq.toList globals }

let trySubstitute v mapping =
    match Map.tryFind v mapping with
    | Some(w) -> w
    | _ -> v

let extendMapping vars mapping =
    let rec loop mapping var =
        if Map.containsKey var mapping then
            loop mapping (freshLabel var)
        else
            var

    let newVars = System.Collections.Generic.List<string>()

    let add mapping var =
        let newVar = loop mapping var
        newVars.Add(newVar)
        Map.add var newVar mapping

    let newMapping = List.fold add mapping vars
    let newVars = Seq.toList newVars

    newVars, newMapping

// TODO: change this to use transform instead
let rec alphaRenameImpl mapping =
    function
    | Ref v -> trySubstitute v mapping |> Ref
    | If(cond, thenExpr, elseExpr) ->
        let cond = alphaRenameImpl mapping cond
        let thenExpr = alphaRenameImpl mapping thenExpr
        let elseExpr = alphaRenameImpl mapping elseExpr
        If(cond, thenExpr, elseExpr)
    | Assign(var, rhs) ->
        let var = trySubstitute var mapping
        let rhs = alphaRenameImpl mapping rhs
        Assign(var, rhs)
    | Lambda(args, dotted, body) ->
        let newVars, mapping = extendMapping args mapping
        let body = List.map (alphaRenameImpl mapping) body
        Lambda(newVars, dotted, body)
    | Begin exprs ->
        let exprs = List.map (alphaRenameImpl mapping) exprs
        Begin(exprs)
    | App(func, args) ->
        let func = alphaRenameImpl mapping func
        let args = List.map (alphaRenameImpl mapping) args
        App(func, args)
    | PrimApp(op, args) ->
        let args = List.map (alphaRenameImpl mapping) args
        PrimApp(op, args)
    | ForeignCall(foreignName, args) ->
        let args = List.map (alphaRenameImpl mapping) args
        ForeignCall(foreignName, args)
    | e -> e

let alphaRename (prog : Program) : Program =
    // let newGlobals, mapping = extendMapping prog.Globals Map.empty
    let initial = prog.Globals @ prog.ConstantsNames
    let initial = List.zip initial initial |> Map.ofList
    let rename = alphaRenameImpl initial
    { prog with Main = List.map rename prog.Main }
                // Globals = newGlobals }

let rec replaceVars mapping expr =
    let transf = replaceVars mapping
    let replace var =
        match Map.tryFind var mapping with
        | Some var2 -> var2
        | _ -> var
    match expr with
    | Ref var -> replace var |> Ref
    | If(cond, conseq, altern) -> If(transf cond, transf conseq, transf altern)
    | Assign(var, rhs) -> Assign(replace var, transf rhs)
    | Lambda(args, dotted, body) -> Lambda(args, dotted, List.map transf body)
    | Begin(exprs) -> Begin(List.map transf exprs)
    | App(func, args) -> App(transf func, List.map transf args)
    | PrimApp(op, args) -> PrimApp(op, List.map transf args)
    | ForeignCall(foreignName, args) -> ForeignCall(foreignName, List.map transf args)
    | e -> e

/// Returns expressions needed for making and initializing
/// a string from literal parameter.
let makeAndInitStringExprs literal =
    let tmp = freshLabel "tmp"

    let assignments =
        literal
        |> Seq.mapi (fun i ch ->
            PrimApp(StringSet, [Ref tmp; Int i; Char ch]))
        |> Seq.toList
    let lambdaBody = assignments @ [Ref tmp]
    let lambda = Lambda([tmp], false, lambdaBody)
    let make = PrimApp(MakeString, [Int (Seq.length literal)])
    App(lambda, [make])

let collectComplexConstants (prog : Program) =
    let exprToName = System.Collections.Generic.Dictionary<Expr, string>()
    let assignments = System.Collections.Generic.List<string * Expr>()
    let strings = System.Collections.Generic.List<string * string>()

    let rec add expr =
        match expr with
        | PrimApp(Cons, [x; y]) ->
            if exprToName.ContainsKey(expr) then
                Ref exprToName.[expr]
            else
                let xx = add x
                let yy = add y
                let thisName = freshLabel ".cconst"
                exprToName.Add(expr, thisName)
                assignments.Add(thisName, PrimApp(Cons, [xx; yy]))
                Ref thisName
        | Symbol s ->
            if exprToName.ContainsKey(expr) then
                Ref exprToName.[expr]
            else
                let sname = add (String s)
                let thisName = freshLabel ".cconst"
                exprToName.Add(expr, thisName)
                let app = App(Ref "string->symbol", [sname])
                assignments.Add(thisName, app)
                Ref thisName
        | String s ->
            if exprToName.ContainsKey(expr) then
                Ref exprToName.[expr]
            else
                let stringName = freshLabel ".string" // This label will hold actual string data.
                let thisName = freshLabel ".cconst" // This label will hold an address of above label.
                exprToName.Add(expr, thisName)
                strings.Add(stringName, s)
                assignments.Add(thisName, PrimApp(StringInit, [Ref stringName]))
                Ref thisName
        | _ -> expr

    let convertHelper propagate transform expr =
        match expr with
        | Quote(Symbol _ as symb) -> add symb
        | Symbol _ | String _ -> add expr
        | Quote((PrimApp(Cons(_), _)) as subExpr) -> add subExpr
        | Quote(_) ->
            failwith "In Core quote can contain only PrimApp(Cons...)"
        | _ -> propagate expr

    let convertExpr expr = transform convertHelper expr

    // Replace all constants with references and collect assignments
    let main = List.map convertExpr prog.Main

    let assignExprs =
        assignments
        |> Seq.map Assign
        |> Seq.toList

    let stringNames = strings |> Seq.map fst |> Seq.toList
    let names = assignments |> Seq.map fst |> Seq.toList

    { prog with Main = assignExprs @ main
                ConstantsNames = names
                Strings = strings |> Seq.toList }

let rec fixArithmeticPrims (prog : Program) : Program =
    let ops = [Add; Mul;]

    let fold op acc curr =
        PrimApp(op, [acc; curr])

    let rec handleExpr propagate transform expr =
        match expr with
        | PrimApp(op, args) when (List.contains op ops) && (args.Length > 1) ->
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
    | Lambda (_, _, body) ->
        findMany body
    | Begin exprs ->
        findMany exprs
    | PrimApp(_, tail) ->
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
    collectComplexConstants
    // >> (fun x -> printfn "%A\n------------------------------------\n" x; x)
    >> fixArithmeticPrims
    >> convertGlobalRefs
    // >> (fun x -> printfn "%A\n------------------------------------\n" x; x)
    >> alphaRename
    >> assignmentConvert
    // >> (fun x -> printfn "%A" x; x)