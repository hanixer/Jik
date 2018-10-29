module Cps

open Base
open Core
open Display

type Var = string

type Cps = 
    | Int of int
    | Bool of bool
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
    | Cps.Int n -> iNum n
    | Cps.Bool true -> iStr "#t"
    | Cps.Bool false -> iStr "#f"
    | Cps.Lambda(free, args, body) -> funclike false "lambda" free args body
    | Cps.LetVal(bindings, body) -> 
        let v, e = bindings
        iConcat [iStr "letval ";
                 iIndent(iConcat([iStr v;
                                  iStr " = ";
                                  showCps e;
                                  iNewline]));
                 iNewline;
                 iStr "in ";
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

/// Conversion Core language -> CPS language
let rec convert expr cont = 
    match expr with
    | Expr.Ref v -> cont v
    | Expr.App(func, args) -> convertApp func args cont
    | Expr.Int n -> 
        let var = freshLabel "num"
        LetVal((var, Int n), cont var)
    | Expr.Bool b -> 
        let var = freshLabel "boo"
        LetVal((var, Bool b), cont var)
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
    let rec accumulateArgs receiveVars vars = 
        function 
        | [] -> 
            let vars = List.rev vars
            receiveVars vars
        | arg :: rest -> 
            convert arg (fun argVar -> 
                accumulateArgs receiveVars (argVar :: vars) rest)

    match func with
    | Expr.Lambda(formals, body) ->
        accumulateArgs (fun vars ->        
            let vars = List.rev vars
            let mapping = 
                List.zip formals vars |> Map.ofList
            let body = replaceVars mapping (Begin body)
            convert body cont) [] args
    | _ ->
        convert func (fun funcVar -> 
            accumulateArgs (fun vars ->      
                let vars = List.rev vars
                let contVar = freshLabel "k"
                let resultVar = freshLabel "callResult"
                LetCont
                    ((contVar, [resultVar], cont resultVar), 
                     Call(contVar, funcVar, vars))) [] args)

and convertIf cond ethen eelse cont = 
    convert cond (fun condVar -> 
        let ifResKVar = freshLabel "ifResk"
        let ifResVar = freshLabel "ifRes"
        let cont2 var =
            ContCall(ifResKVar, [var])
        let thenk = convert ethen cont2
        let elsek = convert eelse cont2
        LetCont((ifResKVar, [ifResVar], cont ifResVar),
            If(condVar, thenk, elsek)))

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
let rec analyzeFreeVars cps =
    match cps with
    | Lambda(freeRef, formals, body) ->
        let free = analyzeFreeVars body
        let free = difference free formals
        freeRef := free
        free
    | LetVal((var, rhs), body) -> 
        let freeRhs = analyzeFreeVars rhs
        let free = difference (analyzeFreeVars body) [var]
        unionFree freeRhs free
    | LetCont((var, formals, contBody), body) -> 
        let freeCont = difference (analyzeFreeVars contBody) formals
        let free = difference (analyzeFreeVars body) [var]
        unionFree freeCont free
    | If(cond, conseq, altern) -> 
        [cond]
        |> unionFree (analyzeFreeVars conseq)
        |> unionFree (analyzeFreeVars altern)
    | Assign(var, rhs) -> [var; rhs]
    | Call(k, f, args) -> k :: f :: args
    | ContCall(k, args) -> k :: args
    | Ref(v) -> [v]
    | Int(_) -> []
    | Return(v) -> [v]
    | PrimCall(_, args) -> args
    | e -> failwithf "analyze free: not implemented for %A" e

type Codes = (Var * Var list * Var list * Cps) list * Cps

// Converts CPS expression into list of first-order 'codes' - procedures,
// which do not contain nested procedures.
// Codes are followed by main expression.
let rec cpsToCodes cps : Codes = 
    let rec transform cps codes =
        match cps with
        | LetVal((var, Lambda(free, formals, lambdaBody)), body) ->
            transformLambda var free formals lambdaBody body codes
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
        | e -> e, codes

    and transformLambda var free formals lambdaBody body codes =
        let body, codes1 = transform body codes
        let lambdaBody, codes2 = transform lambdaBody codes1
        if List.isEmpty !free then
            let code = (var, !free, formals, lambdaBody)
            body, (code :: codes2)
        else
            let codeName = freshLabel "code"
            let codes3 = (codeName, !free, formals, lambdaBody) :: codes2
            MakeClosure(codeName :: !free), codes3

    let e, codes = transform cps []

    codes, e

let rec livenessCps cps =
    1

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


let stringToCps2 = stringToSExpr >> sexprToExpr >> assignmentConvert >> exprToCps