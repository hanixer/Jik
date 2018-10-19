#load "Base.fs"
#load "Util.fs"
#load "Display.fs"

open System
open Base
open Display
open System.Text

// Supported features:
//  - integers
//  - conditionals
//  - assignments
// Assumption for source language: rhs expressions in letrec are lambdas.
// Input:
//  <exp> ::= <literal>
//         |  <variable>
//         |  (if <exp> <exp> <exp>)
//         |  (set! <var> <exp>)
//         |  (lambda (<var>*) <body>)
//         |  (begin <body>)
//         |  (<exp> <exp>*)
//         |  (let ((<var> <exp)*) <body>)
//         |  (letrec ((<var> <exp)*) <body>)
type Expr = 
    | Int of int
    | Ref of string
    | If of Expr * Expr * Expr
    | Assign of string * Expr
    | Lambda of LambdaData
    | Begin of Expr list
    | App of Expr * Expr list
    | LetRec of (string * LambdaData) list * Expr list

and LambdaData = string list * Expr list

let mutable n = 0

let freshLabel s = 
    n <- n + 1
    sprintf "%s%d" s n

let transformLetrecBindings2 bindings = 
    List.map (function 
        | List [Symbol name; List(Symbol "lambda" :: List args :: body)] -> 
            let args = 
                List.map (function 
                    | Symbol n -> n
                    | e -> failwithf "arg is expected") args
            name, (args, body)
        | e -> failwithf "letrec: wrong bindings %A" e) bindings

let rec sexprToExpr sexpr = 
    let convertList = List.map sexprToExpr
    match sexpr with
    | SExpr.Number n -> Int n
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
        let bindings = transformLetrecBindings2 bindings
        let handleLambda(name, (args, body)) = name, (args, convertList body)
        let lambdas = List.map handleLambda bindings
        LetRec(lambdas, convertList body)
    | List(head :: tail) -> App(sexprToExpr head, convertList tail)
    | e -> failwith <| sexprToString e

let trySubstitute v mapping = 
    match Map.tryFind v mapping with
    | Some(w) -> w
    | _ -> v

let extendMapping vars mapping = 
    let newVars = List.map freshLabel vars
    let folder mapping (oldVar, newVar) = Map.add oldVar newVar mapping
    newVars, List.fold folder mapping (List.zip vars newVars)

let rec alphaRename mapping = 
    function 
    | Int _ as e -> e
    | Ref v -> trySubstitute v mapping |> Ref
    | If(cond, thenExpr, elseExpr) -> 
        If
            (alphaRename mapping cond, alphaRename mapping thenExpr, 
             alphaRename mapping elseExpr)
    | Assign(var, rhs) -> 
        Assign(trySubstitute var mapping, alphaRename mapping rhs)
    | Lambda(args, body) -> 
        let newVars, mapping = extendMapping args mapping
        Lambda(newVars, List.map (alphaRename mapping) body)
    | Begin exprs -> Begin(List.map (alphaRename mapping) exprs)
    | App(func, argsExprs) -> 
        App(alphaRename mapping func, List.map (alphaRename mapping) argsExprs)
    | LetRec(bindings, body) -> 
        let vars = List.map fst bindings
        let _, mapping = extendMapping vars mapping
        
        let transform(var, (args, body)) = 
            let var = trySubstitute var mapping
            let args, mappingFunc = extendMapping args mapping
            let body = List.map (alphaRename mappingFunc) body
            var, (args, body)
        
        let bindings = List.map transform bindings
        let body = List.map (alphaRename mapping) body
        LetRec(bindings, body)

let rec findFreeVarsExpr bound = function
    | If (cond, thenc, elsec) ->
        Set.unionMany [
            findFreeVarsExpr bound cond
            findFreeVarsExpr bound thenc
            findFreeVarsExpr bound elsec
        ]
    | Assign (v, rhs) ->
        Set.difference (Set.singleton v) bound
        |> Set.union (findFreeVarsExpr bound rhs) 
    | App (head, tail) ->
        freeVarsExprList bound tail
        |> Set.union (findFreeVarsExpr bound head) 
    | Lambda (vars, body) ->
        freeVarsLambda vars body
    | Begin exprs ->
        freeVarsExprList bound exprs
    | Ref name -> 
        if Set.contains name bound then
            Set.empty
        else 
            Set.singleton name
    | LetRec (bindings, body) ->
        let names = List.map fst bindings
        let folder freeAcc (_, (args, body)) =
            freeVarsLambda args body
            |> Set.union freeAcc
        let freeBindings = List.fold folder Set.empty bindings
        let freeBody = findFreeVarsExpr (Set.union bound (Set.ofSeq names)) (Begin body)
        Set.union freeBindings freeBody
    | _ -> Set.empty

and freeVarsExprList bound exprList =
    let results = List.map (findFreeVarsExpr bound) exprList    
    Set.unionMany results

and freeVarsLambda vars body =
    let bound' = Set.ofList vars
    freeVarsExprList bound' body

type Var = string

type Cps = 
    | Const of int
    | Ref of Var
    | Lambda of CpsLambda
    | LetVal of (Var * Cps) * Cps
    | LetCont of (Var * Var list * Cps) list * Cps
    | LetRec of (Var * CpsLambda) list * Cps
    | If of Var * Cps * Cps
    | Assign of Var * Var
    | Seq of Cps list
    | TailCall of Var * Var list
    | NonTailCall of Var * Var * Var list
    | ContCall of Var * Var list
    | Return of Var
    | MakeClosure of Var list
    | ClosureCall of Var * Var list
    | EnvRef of Var * int

and CpsLambda = ((Var list) ref) * (Var list) * Cps



let rec showCps cps = 
    let funclike needEq s (free : Var list ref) args body = 
        let free = !free
        let v = 
            if needEq then iStr " = "
            else iNil
        iConcat [iStr s;
                 iStr " (";
                 iInterleave (iStr ",") (List.map iStr args);
                 iStr ")";
                 iStr "<";
                 iInterleave (iStr ",") (List.map iStr free);
                 iStr ">";
                 v;
                 iNewline;
                 iStr "  ";
                 iIndent <| showCps body;
                 iNewline]
    
    let letHelper kind bindings body = 
        iConcat [iStr kind;
                 iNewline;
                 iStr "  ";
                 
                 iIndent
                     (iConcat
                          (List.map (fun (v, args, e) -> funclike true v (ref []) args e) 
                               bindings));
                 iNewline;
                 iStr "in ";
                 showCps body]
    
    match cps with
    | Cps.Ref v -> iStr v
    | Cps.Const n -> iNum n
    | Cps.Lambda(free, args, body) -> funclike false "lambda" free args body
    | Cps.LetVal(bindings, body) -> 
        let v, e = bindings
        iConcat [iStr "letval ";
                 iIndent(iConcat([iStr v;
                                  iStr " = ";
                                  showCps e;
                                  iNewline]));
                 iNewline;
                 iStr " in ";
                 showCps body]
    | Cps.LetCont(bindings, body) -> letHelper "letcont " bindings body
    | Cps.LetRec(bindings, body) -> 
        iConcat [iStr "letrec ";
                 iNewline;
                 iStr "  ";
                 
                 iIndent
                     (iConcat
                          (List.map (fun (v, (free, args, e)) -> funclike true v free args e) 
                               bindings));
                 iNewline;
                 iStr "in ";
                 showCps body]
    | Cps.If(c, t, e) -> 
        iIndent(iConcat [iStr "if ";
                         iStr c;
                         iNewline;
                         showCps t;
                         iNewline;
                         showCps e])
    | Cps.Assign(v, e) -> 
        iConcat [iStr v;
                 iStr " <- ";
                 iStr e]
    | Cps.Seq(es) -> List.map showCps es |> iConcat
    | Cps.TailCall(f, v) -> 
        [iStr "tailcall";
         iStr f;
         List.map iStr v |> iInterleave(iStr ",")]
        |> iInterleave(iStr " ")
    | Cps.NonTailCall(k, f, v) -> 
        [iStr "call";
         iStr k;
         iStr f;
         List.map iStr v |> iInterleave(iStr ",")]
        |> iInterleave(iStr " ")
    | Cps.ContCall(k, x) -> 
        [iStr "contcall";
         iStr k;
         List.map iStr x |> iInterleave(iStr ",")]
        |> iInterleave(iStr " ")
    | Cps.Return(v) -> 
        iConcat [iStr "return ";
                 iStr v]
    | Cps.MakeClosure vars -> 
        iConcat [iStr "make-closure ";
                 iInterleave (iStr ",") (List.map iStr vars)]
    | Cps.ClosureCall(func, vars) -> 
        iConcat [iStr "closure-call ";
                 iStr func;
                 iStr " ";
                 iInterleave (iStr ",") (List.map iStr vars)]
    | Cps.EnvRef(var, n) -> 
        iConcat [iStr "env-ref ";
                 iStr var;
                 iStr " ";
                 iNum n]

let cpsToString = showCps >> iDisplay

// Higher-order
//  <exp> ::= (if <simple> <exp> <exp>)
//         |  (set! <var> <simple>)
//         |  (lambda (<var>*) <body>)
//         |  (begin <body>)
//         |  (<simple> <simple>*)
//         |  (let ((<var> <exp)) <body>)
//         |  (letrec ((<var> <exp)*) <body>)
//  <simple> ::= <literal>
//            |  <var>
let rec convert expr cont = 
    match expr with
    | Expr.Ref v -> cont v
    | Expr.App(func, args) -> convertApp func args cont
    | Expr.Int n -> 
        let var = freshLabel "num"
        LetVal((var, Const n), cont var)
    | Expr.If(cond, ethen, eelse) -> convertIf cond ethen eelse cont
    | Expr.Assign(var, rhs) -> convertAssign var rhs cont
    | Expr.Lambda(args, body) -> convertLambda args body cont
    | Expr.Begin(exprs) -> convertBegin exprs cont
    | Expr.LetRec(bindings, body) -> convertLetRec bindings body cont

and convertApp func args cont = 
    convert func (fun funcVar -> 
        let rec loop vars = 
            function 
            | [] -> 
                let vars = List.rev vars
                let contVar = freshLabel "k"
                let resultVar = freshLabel "callResult"
                LetCont
                    ([contVar, [resultVar], cont resultVar], 
                     NonTailCall(contVar, funcVar, vars))
            | arg :: args -> 
                convert arg (fun argVar -> loop (argVar :: vars) args)
        loop [] args)

and convertIf cond ethen eelse cont = 
    convert cond (fun condVar -> 
        let thenk = convert ethen cont
        let elsek = convert eelse cont
        If(condVar, thenk, elsek))

and convertAssign var rhs cont = 
    convert rhs (fun value -> 
        let dummy = freshLabel "unspecified"
        LetVal((dummy, Assign(var, value)), cont dummy))

and convertLambda args body cont = 
    let body = convert (Expr.Begin body) Return
    let var = freshLabel "lam"
    LetVal((var, Lambda (ref [], args, body)), cont var)

and convertBegin exprs cont = 
    let rec loop var = 
        function 
        | expr :: rest -> convert expr (fun var -> loop var rest)
        | [] -> cont var
    loop "" exprs

and convertLetRec bindings body cont = 
    let handleBinding(name, (args, body)) = name, (ref [], args, convertBegin body Return)
    let bindings = List.map handleBinding bindings
    let body = convertBegin body cont
    LetRec(bindings, body)

let exprToCps e =
    convert e Return

let unionFree list1 list2 =
    list1 @ list2
    |> List.distinct

let difference list1 list2 =
    Set.difference (Set.ofSeq list1) (Set.ofSeq list2)
    |> Set.toList

let freeOrNot var bound =
    if List.contains var bound then []
    else [var]

let freeOrNotMany vars bound =
    let folder acc var =
        unionFree acc (freeOrNot var bound)
    List.fold folder [] vars

// takes bound variables and CPS expressions and
// returns free variables in expression
// modifies list of free vars in lambdas
let rec analyzeFreeVars bound cps =
    match cps with
    | Const n -> []
    | Ref v -> freeOrNot v bound
    | Lambda(freeRef, args, body) ->
        analyzeFreeLambda bound (freeRef, args, body)
    | LetVal((var, rhs), body) ->
        let freeRhs = analyzeFreeVars bound rhs
        // let freeRhs = difference (analyzeFreeVars bound rhs) bound
        let freeBody = analyzeFreeVars (unionFree bound [var]) body
        unionFree freeRhs freeBody
    | LetCont(bindings, body) -> 
        let names = List.map (fun (name, _, _) -> name) bindings
        let folder acc (_, args, body) =
            let free = analyzeFreeVars (unionFree bound args) body
            unionFree acc free
        let free = List.fold folder [] bindings
        let freeBody = analyzeFreeVars (unionFree bound names) body
        unionFree free freeBody
    | LetRec(bindings, body) ->
        let names = List.map (fun (name, _) -> name) bindings
        let freeBody = analyzeFreeVars names body
        bindings
        |> List.map (snd >> analyzeFreeLambda (unionFree bound names))
        |> List.fold unionFree freeBody
    | If(cond, conseq, altern) ->
        let freeCond = freeOrNot cond bound
        let freeConseq = analyzeFreeVars bound conseq
        let freeAltern = analyzeFreeVars bound altern
        unionFree freeCond <| unionFree freeConseq freeAltern
    | Assign(var, rhs) ->
        unionFree (freeOrNot var bound) (freeOrNot rhs bound)
    | Seq exprs ->
        let folder acc expr = 
            let free = analyzeFreeVars bound expr
            unionFree acc free
        List.fold folder [] exprs
    | TailCall(func, args) ->
        freeOrNotMany (func :: args) bound
    | NonTailCall(k, func, args) -> 
        freeOrNotMany (k :: func :: args) bound
    | ContCall(k, args) -> 
        freeOrNotMany (k :: args) bound
    | Return(v) -> 
        freeOrNot v bound
    | _ -> failwith "analyzeFreeVars: invalid case"

and analyzeFreeLambda bound (freeRef, args, body) =
    let free = analyzeFreeVars args body
    freeRef := free
    difference free bound

let rec closureConvert cps = 
    match cps with
    | Const n as e -> e
    | Ref v -> Ref v
    | Lambda(_) -> failwith "Not Implemented"
    | LetVal(binding, body) ->
        failwith "Not Implemented"
    | LetCont(_, _) -> failwith "Not Implemented"
    | LetRec(_, _) -> failwith "Not Implemented"
    | If(_, _, _) -> failwith "Not Implemented"
    | Assign(_, _) -> failwith "Not Implemented"
    | Seq(_) -> failwith "Not Implemented"
    | TailCall(_, _) -> failwith "Not Implemented"
    | NonTailCall(_, _, _) -> failwith "Not Implemented"
    | ContCall(_, _) -> failwith "Not Implemented"
    | Return(_) -> failwith "Not Implemented"
    | MakeClosure(_) -> failwith "Not Implemented"
    | ClosureCall(_, _) -> failwith "Not Implemented"
    | EnvRef(_, _) -> failwith "Not Implemented"

let tryit s = 
    stringToSExpr s
    |> sexprToExpr
    |> exprToCps
    |> cpsToString
    |> printfn "%s"

let tryRename s = 
    stringToSExpr s
    |> sexprToExpr
    |> alphaRename Map.empty
    |> printfn "%A"

let testFree s =
    let cps =
        stringToSExpr s
        |> sexprToExpr
        |> exprToCps
    printfn "before:\n%A\n\n\nafter:" cps
    printfn "%A\n\n---\n\n%A" (analyzeFreeVars [] cps) cps

"(letrec (
(sort (lambda (lst)
    (if (pair? lst)
        (insert (car lst) (sort (cdr lst)))
        1)))
(insert (lambda (elem lst)
    (if (pair? lst)
        (let ((x (car lst))
            (l (cdr lst)))
            (if (< elem x)
                (cons elem (cons x l))
                (cons x (insert elem l))))
        (cons elem 1)))))
    (sort (cons 333 (cons 222 (cons 111 1)))))" |> testFree
"1"
"(let ((f (lambda (a) (lambda (b) (+ a b)))))
    ((f 1) 2))"
"(lambda (a) (lambda (b) (lambda (c) (+ a b c))))"
"(lambda (a) (lambda (b) (opera a b)))"
"(letrec ((f (lambda (x) (opera f g x)))
          (g (lambda (y) (opera f g y))))
    1)" 