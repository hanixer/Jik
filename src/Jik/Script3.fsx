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
type Prim =
    | BoxRead
    | BoxWrite
    | BoxCreate

type Expr = 
    | Int of int
    | Ref of string
    | If of Expr * Expr * Expr
    | Assign of string * Expr
    | Lambda of LambdaData
    | Begin of Expr list
    | App of Expr * Expr list
    | PrimApp of Prim * Expr list

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
    | PrimApp(op, args) ->
        PrimApp(op, List.map (alphaRename mapping) args)

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
    let alpha = alphaRename Map.empty expr
    let modified = findModifiedVars alpha
    let rec transform = function
        | Int n -> Int n
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
    and boxifyLambda args body =
        let folder var body =
            if modified.Contains var then
                PrimApp(BoxCreate, [Ref var]) :: body
            else
                body
        let body = List.foldBack folder args (List.map transform body)
        Lambda(args, body)
    transform alpha

type Var = string

type Cps = 
    | Const of int
    | Ref of Var
    | Lambda of CpsLambda
    | LetVal of (Var * Cps) * Cps
    | LetCont of (Var * Var list * Cps) * Cps
    | If of Var * Cps * Cps
    | Assign of Var * Var
    | Call of Var * Var * Var list
    | ContCall of Var * Var list
    | Return of Var
    | MakeClosure of Var list
    | ClosureCall of Var * Var list
    | EnvRef of int
    | PrimCall of Prim * Var list

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
    | PrimCall(op, args) ->
        iConcat [iStr "prim-call "
                 iStr (sprintf "%A " op)
                 iInterleave (iStr ",") (List.map iStr args); iNewline]

let cpsToString = showCps >> iDisplay

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
    | Expr.PrimApp(op, args) ->
        let rec loop vars = 
            function 
            | [] -> 
                let vars = List.rev vars
                let var = freshLabel "primResult"
                LetVal((var, PrimCall(op, vars)), cont var)
            | arg :: args -> 
                convert arg (fun argVar -> loop (argVar :: vars) args)
        loop [] args

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

// takes CPS expressions and
// returns free variables in expression
// modifies list of free vars in lambdas
let rec analyzeFreeVars2 cps =
    match cps with
    | Lambda(freeRef, formals, body) ->
        let free = analyzeFreeVars2 body
        let free = difference free formals
        freeRef := free
        free
    | LetVal((var, rhs), body) -> 
        let freeRhs = analyzeFreeVars2 rhs
        let free = difference (analyzeFreeVars2 body) [var]
        unionFree freeRhs free
    | LetCont((var, formals, contBody), body) -> 
        let freeCont = difference (analyzeFreeVars2 contBody) formals
        let free = difference (analyzeFreeVars2 body) [var]
        unionFree freeCont free
    | If(cond, conseq, altern) -> 
        [cond]
        |> unionFree (analyzeFreeVars2 conseq)
        |> unionFree (analyzeFreeVars2 altern)
    | Assign(var, rhs) -> [var; rhs]
    | Call(k, f, args) -> k :: f :: args
    | ContCall(k, args) -> k :: args
    | Ref(v) -> [v]
    | Const(_) -> []
    | Return(v) -> [v]
    | PrimCall(_, args) -> args
    | e -> failwithf "analyze free: not implemented for %A" e

type Codes = (Var * Var list * Var list * Cps) list * Cps

// Converts CPS expression into list of first-order 'codes' - procedures,
// followed by main expression.
let rec cpsToCodes cps : Codes = 
    let rec transform cps codes =
        match cps with
        | Lambda(free, formals, body) ->
            let body, codes1 = transform body codes
            let codeName = freshLabel "code"
            let code = (codeName, !free, formals, body)
            MakeClosure(codeName :: !free), (code :: codes1)
        | LetVal((var, rhs), body) ->
            let rhs, codes1 = transform rhs codes
            let body, codes2 = transform body codes1
            LetVal((var, rhs), body), codes2            
        | LetCont((var, args, contBody), body) -> 
            let contBody, codes1 = transform contBody codes
            let body, codes2 = transform body codes1
            LetCont((var, args, contBody), body), codes2
        | If(cond, conseq, altern) ->
            let conseq, codes1 = transform conseq codes
            let altern, codes2 = transform altern codes1
            If(cond, conseq, altern), codes2
        | _ as e -> e, codes
    let e, codes = transform cps []
    codes, e

let showCommaSep strings =
    iInterleave (iStr ",") (List.map iStr strings)

let showCodes (codes, cps) =
    let codes =
      List.map (fun (name, free, formals, body) ->
        iConcat 
            [iStr name; 
             iStr " <"; 
             showCommaSep free;
             iStr "> ";
             iStr "("
             showCommaSep formals;
             iStr ")"; iNewline;
             showCps body; iNewline; iStr "--------"; iNewline]) 
        codes
    iAppend (iConcat codes) (showCps cps)

let codesToString = showCodes >> iDisplay

let stringToExpr = stringToSExpr >> sexprToExpr

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

let test s =
    let cps =
        s
        |> stringToSExpr
        |> sexprToExpr
        |> assignmentConvert
        |> exprToCps
    let _ = analyzeFreeVars2 cps
    cps
    |> cpsToCodes
    |> codesToString
    |> printfn "%s"

let testModified s =
    s
    |> stringToExpr
    |> findModifiedVars
    |> printfn "%A"
    

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
"(lambda (a) (lambda (b) (opera a b)))"
"(letrec ((f (lambda (x) (opera f g x)))
          (g (lambda (y) (opera f g y))))
    (let ((x 0))
        (set! x (+ x 1))
        (if x 1234 4321)))"
"(let ((x 0))
        (set! x (+ x 1))
        (if x 1234 4321))"
"(letrec ((fact (lambda (x) 
    (if (< x 2)
        x
        (* x (fact (- x 1)))))))
  (fact 5))" |> test