#load "Base.fs"
open System.Collections.Generic
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
// Input:
//  <exp> ::= <literal>
//         |  <variable>
//         |  (if <exp> <exp> <exp>)
//         |  (set! <var> <exp>)
//         |  (lambda (<var>*) <body>)
//         |  (begin <body>)
//         |  (<exp> <exp>*)
//         |  (let ((<var> <exp)*) <body>)
type Expr = 
    | Int of int
    | Ref of string
    | If of Expr * Expr * Expr
    | Assign of string * Expr
    | Lambda of LambdaData
    | Begin of Expr list
    | App of Expr * Expr list

and LambdaData = string list * Expr list

let freshLabel = 
    let mutable dict  = Dictionary<string, int>()
    fun prefix ->
        if dict.ContainsKey prefix |> not then
            dict.Add(prefix, 0)
        let count = dict.Item prefix
        dict.Item prefix <- count + 1
        sprintf "%s%d" prefix count

let freshCodeLabel prefix = freshLabel (prefix + "/code")

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

type Var = string

type Cps = 
    | Const of int
    | Ref of Var
    | Lambda of CpsLambda
    | Code of (Var list) * Cps
    | LetVal of (Var * Cps) * Cps
    | LetCont of (Var * Var list * Cps) * Cps
    | If of Var * Cps * Cps
    | Assign of Var * Var
    | Seq of Cps list
    | Call of Var * Var * Var list
    | ContCall of Var * Var list
    | Return of Var
    | MakeClosure of Var list
    | ClosureCall of Var * Var list
    | EnvRef of int

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
    | Cps.LetCont(bindings, body) -> 
        let v, args, e = bindings
        iConcat [iStr "letcont ";
                 iNewline;
                 iStr "  ";                 
                 iIndent (funclike true v (ref []) args e);
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
    | Call(k, f, v) -> 
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
    | Cps.EnvRef(n) -> 
        iConcat [iStr "env-ref ";
                 iStr " ";
                 iNum n]
    | Code(_, _) -> failwith "Not Implemented"

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

and convertApp func args cont = 
    convert func (fun funcVar -> 
        let rec loop vars = 
            function 
            | [] -> 
                let vars = List.rev vars
                let contVar = freshLabel "k"
                let resultVar = freshLabel "callResult"
                LetCont
                    ((contVar, [resultVar], cont resultVar), 
                     Call(contVar, funcVar, vars))
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
        let freeBody = analyzeFreeVars (unionFree bound [var]) body
        unionFree freeRhs freeBody
    | LetCont((name, args, contBody), body) -> 
        let free = analyzeFreeVars (unionFree bound args) body
        let freeBody = analyzeFreeVars (unionFree bound [name]) body
        unionFree free freeBody
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
    | Call(k, func, args) -> 
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
    let t = closureConvert
    match cps with
    | Const n as e -> e
    | Ref v -> Ref v
    | Lambda(free, args, body) ->
        Lambda (free, args, t body)
    | LetVal((var, Lambda(lambda)), body) ->
        closureConvertLambda var lambda body
    | LetVal((var, rhs), body) ->
        LetVal((var, t rhs), t body)
    | LetCont((var, args, contBody), body) -> 
        LetCont((var, args, contBody), body)
    | If(cond, conseq, altern) ->
        failwith "Not Implemented"
    | Assign(_, _) -> failwith "Not Implemented"
    | Seq(_) -> failwith "Not Implemented"
    | Call(_, _, _) -> failwith "Not Implemented"
    | ContCall(_, _) -> failwith "Not Implemented"
    | Return(_) -> failwith "Not Implemented"
    | MakeClosure(_) -> failwith "Not Implemented"
    | ClosureCall(_, _) -> failwith "Not Implemented"
    | EnvRef(_) -> failwith "Not Implemented"

and closureConvertLambda var (free, args, lambdaBody) body =
    let varCode = freshCodeLabel var
    LetVal((varCode, Code(args, lambdaBody)), 
        LetVal((var, MakeClosure(varCode :: !free)), 
            closureConvert body))

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
    (sort (cons 333 (cons 222 (cons 111 1)))))"
"1"
"(let ((f (lambda (a) (lambda (b) (+ a b)))))
    ((f 1) 2))"
"(lambda (a) (lambda (b) (lambda (c) (+ a b c))))"
"(lambda (a) (lambda (b) (opera a b)))"|> testFree
"(letrec ((f (lambda (x) (opera f g x)))
          (g (lambda (y) (opera f g y))))
    1)" 