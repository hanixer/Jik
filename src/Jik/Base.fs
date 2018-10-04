module Base

open System
open System.Text

type Token =
    | Open | Close
    | OpenBr | CloseBr
    | Number of string
    | String of string
    | Symbol of string
    | Bool of bool
    | Char of char
    | Dot
    | Quote
    | Unquote

let specialChars = [
    "tab", '\t' 
    "newline", '\n' 
    "return", '\n' 
]

let tokenize source =
    let isSymbolChar c =
        (Char.IsWhiteSpace(c) ||
         c = '(' ||
         c = ')')
        |> not

    let rec string (acc:string) = function
        | '\\' :: '"' :: cs -> string (acc + "\"") cs
        | '"' :: cs -> acc, cs
        | c :: cs -> string (acc + c.ToString()) cs
        | [] -> failwith "EOF not espected"

    let character cs =
        let found =
            List.tryPick (fun (s, c) ->
                let l = Seq.length s
                if Seq.toList s = List.truncate l cs then
                    Some (c, (List.skip l cs))
                else None) specialChars
        match found, cs with
        | Some (c, cs'), _ -> Char c, cs'
        | _, (c::cs') -> Char c, cs'
        | _ -> failwith "tokenize: character: EOF not expected"


    let rec number (acc:string) = function
        | d :: cs when Char.IsDigit(d) ->
            number (acc + d.ToString()) cs
        | cs -> acc, cs

    let rec symbol (acc:string) = function
        | d :: cs when isSymbolChar d ->
            symbol (acc + d.ToString()) cs
        | cs ->
            acc, cs

    let rec loop acc = function
        | w :: cs when System.Char.IsWhiteSpace(w) -> loop acc cs
        | '(' :: cs -> loop (Open :: acc) cs
        | ')' :: cs -> loop (Close :: acc) cs
        | '[' :: cs -> loop (OpenBr :: acc) cs
        | ']' :: cs -> loop (CloseBr :: acc) cs
        | '"' :: cs -> 
            let s, cs' = string "" cs
            loop (String s :: acc) cs'
        | '-' :: d ::cs when Char.IsDigit(d) ->
            let n, cs' = number "-" (d::cs)
            loop (Number n :: acc) cs'
        | '+' :: d ::cs when Char.IsDigit(d) ->
            let n, cs' = number "" (d::cs)
            loop (Number n :: acc) cs
        | '#' :: 't' :: cs | '#' :: 'T' :: cs ->
            loop (Bool true :: acc) cs
        | '#' :: 'f' :: cs | '#' :: 'F' :: cs ->
            loop (Bool false :: acc) cs
        | '#' :: '\\' :: cs ->
            let c, cs' = character cs
            loop (c :: acc) cs'
        | '.' :: cs -> loop (Dot :: acc) cs
        | '\'' :: cs -> loop (Quote :: acc) cs
        | ',' :: cs -> loop (Unquote :: acc) cs
        | d ::cs when Char.IsDigit(d) ->
            let n, cs' = number "" (d::cs)
            loop (Number n :: acc) cs'
        | [] -> acc
        | cs ->
            let s, cs' = symbol "" cs
            loop (Symbol s :: acc) cs'
    
    loop [] (Seq.toList source) |> List.rev


type SExpr =
    | Cons of SExpr * SExpr
    | Nil
    | Number of int
    | String of string
    | Char of char
    | Symbol of string
    | Bool of bool
    | BuiltinFunc of (SExpr list -> SExpr)
and Frame = Map<string, SExpr ref> ref

let (|List|_|) (e:SExpr) =
    let rec loop = function
        | Cons (x, y) ->
            loop y
            |> Option.map (fun y -> x :: y)
        | Nil -> Some []
        | _ -> None
    loop e


/// **Description**
///   Converts expession list in consed list : [1;2;3] -> Cons(1, Cons(2, Cons(3, Nil)))
/// **Parameters**
///   * `exprs` - parameter of type `SExpr list`
///
/// **Output Type**
///   * `SExpr`
///
/// **Exceptions**
///
let exprsToList exprs =
    List.foldBack (fun x y -> Cons (x, y)) exprs Nil

/// **Description**
///   Converts expression list in conses, included dotted conses : [1;2;3] -> Cons(1, Cons(2, 3))
/// **Parameters**
///   * `_arg1` - parameter of type `SExpr list`
///
/// **Output Type**
///   * `SExpr`
///
/// **Exceptions**
///
let rec exprsToDottedList = function
    | [Nil] -> Nil
    | x :: [y] -> Cons (x, y)
    | x :: xs -> Cons (x, exprsToDottedList xs)
    | [] -> failwith "Not expected for buildConses"


/// **Description**
///   Converts conses to expression list. Cons(1, Cons(2, Nil)) -> [1;2]
/// **Parameters**
///   * `_arg1` - parameter of type `SExpr`
///
/// **Output Type**
///   * `SExpr list`
///
/// **Exceptions**
///
let rec consToList = function
    | Cons (x, y) ->
        x :: (consToList y)
    | Nil -> []
    | e -> [e]

let rec consToListMap f = function
    | Cons (x, y) ->
        (f x) :: (consToListMap f y)
    | Nil -> []
    | _ -> failwith "Cons or Nil is expected"

let rec parse ts : SExpr =
    let rec parseList o (acc:SExpr list) ts = 
        match ts with
        | Dot :: ts ->
            let elem, ts1 = parseExpr ts
            match ts1 with
            | Close :: ts2 -> exprsToDottedList (List.rev (elem :: acc)), ts2
            | _ -> failwith "Close paren is expected after dot"
        | Close :: ts ->
            if o = Open then
                exprsToDottedList (List.rev (Nil :: acc)), ts
            else failwith "Close paren expected"
        | Open :: ts ->
            let subList, ts1 = parseList Open [] ts
            parseList o (subList :: acc) ts1
        | CloseBr :: ts ->
            if o = OpenBr then
                exprsToDottedList (List.rev (Nil :: acc)), ts
            else failwith "Close bracket expected"
        | OpenBr :: ts ->
            let subList, ts1 = parseList OpenBr [] ts
            parseList o (subList :: acc) ts1
        | ts ->
            let elem, ts1 = parseNonlist ts
            parseList o (elem :: acc) ts1

    and parseNonlist ts =
        match ts with
        | Token.Number n :: ts ->
            (SExpr.Number (int n)), ts
        | Token.Symbol n :: ts ->
            (SExpr.Symbol (n.ToLower())), ts
        | Token.String n :: ts ->
            (SExpr.String n), ts
        | Token.Bool n :: ts ->
            (SExpr.Bool n), ts
        | Token.Char c :: ts ->
            (SExpr.Char c), ts
        | Quote :: ts ->
            let expr, ts1 = parseExpr ts
            Cons (Symbol "quote", Cons (expr, Nil)), ts1
        | Unquote :: ts ->
            let expr, ts1 = parseExpr ts
            Cons (Symbol "unquote", Cons (expr, Nil)), ts1
        | ts -> failwithf "Expected a list element. %A" ts

    and parseExpr ts = 
        match ts with
        | Open :: ts1 ->
            parseList Open [] ts1
        | OpenBr :: ts1 ->
            parseList OpenBr [] ts1
        | _ -> parseNonlist ts

    parseExpr ts |> fst

let rec sexprToString expr =
    let s = StringBuilder()
    let add (str:string) = s.Append(str) |> ignore
    let rec loop = function
        | Number n -> n.ToString() |> add
        | String str -> 
            add "\""
            str.Replace("\"", "\\\"") |> add
            add "\""
        | Symbol sy -> add sy
        | Bool true -> add "#t"
        | Bool false -> add "#f"
        | Char c -> 
            add "#\\"
            add <| c.ToString()
        | Cons _ as c ->
            add "("
            processCons c
            add ")"
        | BuiltinFunc f ->  add (sprintf "BuiltinFunc %A" f)
        | Nil -> add "()"
    and processCons = function
        | Cons (x, y) ->
            match y with
            | Cons _ ->
                loop x
                add " "
                processCons y
            | Nil -> 
                loop x
            | _ ->
                loop x
                add " . "
                loop y
        | _ -> failwith "Cons is expected"
    and printLambdaMacro name args body=
        let args = List.map Symbol args
        [ yield Symbol name
          yield! args
          yield body ]
        |> exprsToList
        |> sexprToString
        |> add
    loop expr
    s.ToString()

let define s env value =
    match env with
    | first :: rest ->
        first := Map.add s (ref value) first.Value
    | [] -> failwith "non-empty environment is expected for define"

let extend env bindings =
    let frame = List.fold (fun acc (n, v) -> Map.add n (ref v) acc) Map.empty bindings
    (ref frame) :: env

let getTopEnv env = 
    [List.last env]

let stringToSExpr s = s |> tokenize |> parse

let symbolsToStrings sexprs =
    List.map (function
        | Symbol name -> name
        | _ -> failwith "symbolsToStrings: Symbol expected") sexprs

let transformLetrecBindings bindings =
  List.map (function
    | List [Symbol name; List [Symbol "lambda"; List args; body]] -> 
        let args = List.map (function | Symbol n -> n | e -> failwithf "arg is expected") args
        name, (args, body)
    | e -> failwithf "letrec: wrong bindings %A" e) bindings