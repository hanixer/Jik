module Compile

open Base
open System.Text

let wordSize = 8
let fixnumShift = 2
let fixnumMask = 0x03
let fixnumTag = 0x00
let falseLiteral = 0x2F
let trueLiteral = 0x6F
let boolBit = 6
let boolTag = 0x2F
let charShift = 8
let charTag = 0x0F
let charMask = 0xFF
let nilLiteral = 0x8F

let pairTag = 0x01
let pairMask = 0x07
let pairSize = 2 * wordSize
let carOffset = 0
let cdrOffset = wordSize

let vectorTag = 0x05
let vectorMask = 0x07
let stringTag = 0x06
let stringMask = 0x07

let freshLabel =
    let mutable n = 0
    fun () ->
        n <- n + 1
        sprintf "L_%d" n

let tryGetLabel name env = 
    Map.tryFind name env 

let getLabel name env =
    match tryGetLabel name env with
    | (Some (Choice2Of2 label)) -> label
    | _ -> failwithf "getLabel: label '%s' not found" name

let isCall env = function
    | List (Symbol "app" :: Symbol name :: exprs) -> Map.containsKey name env
    | Cons (Symbol name, _) -> Map.containsKey name env
    | _ -> false

let getLabelAndArgs env = function
    | List (Symbol "app" :: Symbol name :: exprs) 
    | Cons (Symbol name, List exprs) -> 
        getLabel name env, exprs
    | _ -> failwith "getLabelAndArgs: wrong sexpr"


let align n =
    (n + (wordSize - 1)) / wordSize * wordSize

let immediateRep e =
    match e with
    | Number n -> n <<< fixnumShift
    | Bool false -> falseLiteral
    | Bool true -> trueLiteral
    | Char c -> ((int c) <<< charShift) ||| charTag
    | Nil -> nilLiteral
    | _ -> failwith "expected immediate"

let compile s =
    let mainExpr = stringToSExpr s
    let sb = new StringBuilder()
    let emitn s = sb.AppendLine s |> ignore
    let emit (s:string) = sb.Append s |> ignore
    let emitf f = Printf.kprintf emit f
    let emitfn f = Printf.kprintf emitn f

    let emitLoadNumber (n:int) = 
        emitfn "  mov $%d, %%rax" n
    let emitToBool () =
        emitfn "  movzb %%al, %%rax"
        emitfn "  sal $%d, %%al" boolBit
        emitfn "  or $%d, %%al" falseLiteral
    let emitToBoolEq () =    
        emitfn "  sete %%al"
        emitToBool ()
    let emitCompare flag si =
        emitfn "  cmp %d(%%rsp), %%rax" si
        emitfn "  set%s %%al" flag
        emitToBool ()

    let rec emitMainExpr = function
        | List [Symbol "letrec"; List bindings; body] ->
            ``letrec`` Map.empty 0 bindings body
        | mainExpr ->  
            emitSchemeEntry Map.empty -wordSize mainExpr

    and ``letrec`` env si bindings body =
        let bindings = List.map (function
            | List [Symbol name; List [Symbol "lambda"; List args; body]] -> 
                let args = List.map (function | Symbol n -> n | e -> failwithf "arg is expected") args
                name, (args, body)
            | e -> failwithf "letrec: wrong bindings %A" e) bindings
        let names = List.map fst bindings        
        let newEnv = 
            List.fold (fun newEnv name -> 
                Map.add name (freshLabel() |> Choice2Of2) newEnv) 
                env names
        List.iter (fun (name, (args, body)) -> 
            let label = getLabel name newEnv
            emitFunction label newEnv args body) bindings   
        emitSchemeEntry newEnv (si - wordSize) body
    
    and emitSchemeEntry env si body = 
        emitn "  .text"
        emitn "  .globl schemeEntry"
        emitn "  .def schemeEntry; .scl 2; .type 32; .endef"
        emitn "  .seh_proc schemeEntry"
        emitn "schemeEntry:"
        emitn "  pushq %rbp"
        emitn "  .seh_pushreg %rbp"
        emitn "  movq %rsp, %rbp"
        emitn "  .seh_setframe %rbp, 0"
        emitn "  .seh_endprologue"
        emitn "  push %rbp"
        emitn "  push %rbx"
        emitn "  push %rsi"
        emitn "  push %rdi"
        emitn "  push %r15"
        emitn "  mov %rsp, %r15"
        emitn "  mov %rcx, %rsp"   
        emitn "  mov %rdx, %rbp"
        emitExpr env si body
        emitn "  mov %r15, %rsp"
        emitn "  pop %r15"
        emitn "  pop %rdi"
        emitn "  pop %rsi"
        emitn "  pop %rbx"
        emitn "  popq %rbp"
        emitn "  popq %rbp"
        emitn "  ret"
        emitn "  .seh_endproc"

    and emitExpr env si = function
        | List [Symbol "if"; cond; th; el] ->
            ``if`` false env si cond th el
        | List [Symbol "let"; List bindings; body] ->
            ``let`` env si bindings body
        | List (Symbol "let" :: List bindings :: bodyExprs) ->
            ``let`` env si bindings (exprsToList (Symbol "begin" :: bodyExprs))
        | List [Symbol "letrec"; List bindings; body] ->
            ``letrec`` env si bindings body
        | List (Symbol "begin" :: exprs) ->
            ``begin`` env si exprs
        | List (Symbol "app" :: Symbol name :: exprs) ->
            match tryGetLabel name env with
            | Some (Choice2Of2 label) -> emitApp env si label exprs
            | _ -> failwithf "app: label '%s' not found" name
        | List [Symbol "vector-set!"; vector; index; value] ->
            ``vector-set!`` env si vector index value
        | List [Symbol "string-set!"; string; index; value] ->
            ``string-set!`` env si string index value
        | Cons (Symbol s, List args) ->
            match tryGetLabel s env with
            | Some (Choice2Of2 label) -> emitApp env si label args
            | _ -> emitPrimitive s args env si
        | Symbol s ->
            emitVariableRef env s
        | e ->
            immediateRep e |> emitLoadNumber

    and emitExprPossibleTail isTail env si expr =
        if isTail then emitExprTail env si expr
        else emitExpr env si expr

    and emitExprTail env si = function
        | List [Symbol "if"; cond; th; el] ->
            ``if`` true env si cond th el
        | call when isCall env call ->
            emitAppTail env si call
        | expr ->
            emitExpr env si expr
            emitEpilogue ()

    and ``if`` isTail env si cond th el =
        let elseLabel = freshLabel ()
        let endLabel = freshLabel ()
        emitExpr env si cond
        emitfn "  cmp $%d, %%rax" falseLiteral
        emitfn "  je %s" elseLabel
        if isTail then
            emitExprTail env si th
        else
            emitExpr env si th
            emitfn "  jmp %s" endLabel
        emitfn "%s:" elseLabel
        if isTail then
            emitExprTail env si el
        else
            emitExpr env si el
            emitfn "%s:" endLabel

    and ``let`` env si bindings body =
        let processBinding (newEnv, si) = function
            | List [Symbol name; initExpr] ->
                emitExpr env si initExpr

                emitfn "  mov %%rax, %d(%%rsp)" si
                Map.add name (Choice1Of2 si) newEnv, si - wordSize
            | e -> failwithf "let: processBinding: wrong binding: %A" e

        let newEnv, newSi = List.fold processBinding (env, si) bindings
        emitExpr newEnv newSi body

    and ``begin`` env si exprs =
        if exprs.IsEmpty then 
            failwith "begin: cannot be empty"
        List.iter (fun expr -> emitExpr env si expr) exprs

    and emitPrologue name =
        emitfn "  .text"
        emitfn "  .globl %s" name
        emitfn "%s:" name

    and emitEpilogue () =
        emitfn "  ret"

    and emitFunction name env args body =
        let extendEnv (newEnv, si) arg = 
            Map.add arg (Choice1Of2 si) newEnv, (si - wordSize)
        let newEnv, newSi = List.fold extendEnv (env, -wordSize) args
        emitPrologue name
        emitExprTail newEnv newSi body

    and emitArgs env si exprs =
        let emitArg si expr =
            emitExpr env si expr
            emitfn "  mov %%rax, %d(%%rsp)" si
            si - wordSize
        let _ = List.fold emitArg (si - wordSize) exprs
        ()

    and emitApp env si label exprs = 
        let newSi = si + wordSize
        emitArgs env si exprs
        emitfn "  add $%d, %%rsp" newSi
        emitfn "  call %s" label
        emitfn "  sub $%d, %%rsp" newSi

    and emitAppTail env si call =
        let label, args = getLabelAndArgs env call
        let argsCount = args.Length
        let rec moveArgs i fromSi toSi =
            if i < argsCount then
                emitfn "  mov %d(%%rsp), %%rax" fromSi
                emitfn "  mov %%rax, %d(%%rsp)" toSi
                moveArgs (i + 1) (fromSi - wordSize) (toSi - wordSize)
        let newSi = si + wordSize
        emitArgs env newSi args
        moveArgs 0 si -wordSize
        emitfn "  jmp %s" label

    and emitVariableRef env name =
        match Map.tryFind name env with
        | Some (Choice1Of2 si) -> emitfn "  mov %d(%%rsp), %%rax " si
        | _ -> failwithf "emitVariableRef: variable '%s' not found" name

    and emitPrimitive s args env si  =
        match findPrimitive s with
        | Some (_, argsCount, emitter) ->
            if argsCount <> args.Length then
                failwithf "primitive <%s> wrong argument count: must be<%d> but was <%d>" s argsCount args.Length
            emitter env si args
        | None ->
            failwithf "primitive '%s' not found" s

    and emitUnary em env si args  =
        let arg = List.head args
        emitExpr env si arg
        em ()

    and emitBinary em env si args =
        match args with
        | [arg1; arg2] ->
            emitExpr env si arg1
            emitfn "  mov %%rax, %d(%%rsp)" si
            emitExpr env (si - wordSize) arg2
            em env si
        | _ -> failwith "binary primitive: wrong arguments"

    and ``fxadd1`` () =
        emitfn "  add $%d, %%rax" (immediateRep <| Number 1)
    and ``fxsub1`` () =
        emitfn "  sub $%d, %%rax" (immediateRep <| Number 1)
    and ``fixnum->char`` () =
        emitfn "  sar $%d, %%rax" fixnumShift
        emitfn "  sal $%d, %%rax" charShift
        emitfn "  or $%d, %%rax" charTag
    and ``char->fixnum`` () =
        emitfn "  sar $%d, %%rax" charShift
        emitfn "  movzb %%al, %%rax"
        emitfn "  sal $%d, %%rax" fixnumShift
    and ``fixnum?`` () =
        emitfn "  and $%d, %%al" fixnumMask
        emitfn "  cmp $%d, %%al" fixnumTag
        emitToBoolEq ()
    and ``boolean?`` () =
        emitfn "  and $%d, %%al" boolTag
        emitfn "  cmp $%d, %%al" boolTag
        emitToBoolEq ()
    and ``null?`` () =
        emitfn "  cmp $%d, %%rax" nilLiteral
        emitToBoolEq ()
    and ``zero?`` () =
        emitfn "  cmp $0, %%rax"
        emitToBoolEq ()
    and ``char?`` () =
        emitfn "  cmp $%d, %%al" charTag
        emitToBoolEq ()
    and ``pair?`` () =
        emitfn "  and $%d, %%al" pairMask
        emitfn "  cmp $%d, %%al" pairTag
        emitToBoolEq ()
    and ``vector?`` () =
        emitfn "  and $%d, %%al" vectorMask
        emitfn "  cmp $%d, %%al" vectorTag
        emitToBoolEq ()
    and ``string?`` () =
        emitfn "  and $%d, %%al" stringMask
        emitfn "  cmp $%d, %%al" stringTag
        emitToBoolEq ()
    and ``not`` () =
        emitfn "  cmp $%d, %%rax" falseLiteral
        emitToBoolEq ()
    and ``fxlognot`` () =
        emitfn "  sar $%d, %%rax" fixnumShift
        emitfn "  not %%rax"
        emitfn "  sal $%d, %%rax" fixnumShift
    and car () =
        emitfn "  sub $%d, %%rax" pairTag
        emitfn "  mov %d(%%rax), %%rax" carOffset
    and cdr () =
        emitfn "  sub $%d, %%rax" pairTag
        emitfn "  mov %d(%%rax), %%rax" cdrOffset

    and ``make-vector`` () =
        emitfn "  mov %%rax, (%%rbp)"
        emitfn "  mov %%rax, %%rcx"
        emitfn "  mov %%rbp, %%rax"
        emitfn "  or $%d, %%rax" vectorTag
        emitfn "  sar $%d, %%rcx" fixnumShift
        emitfn "  leaq %d(%%rbp, %%rcx, %d), %%rbp" wordSize wordSize
    and ``vector-length`` () =
        emitfn "  sub $%d, %%rax" vectorTag
        emitfn "  mov (%%rax), %%rax"
    and ``vector-set!`` env si vector index value =
        emitExpr env si vector
        emitfn "  mov %%rax, %%rcx"
        emitExpr env si index
        emitfn "  sar $%d, %%rax" fixnumShift
        emitfn "  add $1, %%rax" 
        emitfn "  mov %%rax, %%rdx"
        emitExpr env si value
        emitfn "  mov %%rax, -%d(%%rcx, %%rdx, %d)" vectorTag wordSize
    and ``vector-ref`` env si =
        emitfn "  sar $%d, %%rax" fixnumShift
        emitfn "  add $1, %%rax"
        emitfn "  mov %%rax, %%rdx"
        emitfn "  mov %d(%%rsp), %%rcx" si
        emitfn "  mov -%d(%%rcx, %%rdx, %d), %%rax" vectorTag wordSize

    and ``make-string`` () =
        emitfn "  mov %%rax, (%%rbp)"
        emitfn "  sar $%d, %%rax" fixnumShift
        emitfn "  sub $1, %%rax"
        emitfn "  mov $%d, %%rcx" wordSize
        emitfn "  xor %%rdx, %%rdx"
        emitfn "  div %%rcx"
        emitfn "  add $2, %%rax"
        emitfn "  mov %%rax, %%rcx"
        emitfn "  mov %%rbp, %%rax"
        emitfn "  or $%d, %%rax" stringTag
        emitfn "  leaq (%%rbp, %%rcx, %d), %%rbp" wordSize
    and ``string-length`` () =
        emitfn "  sub $%d, %%rax" stringTag
        emitfn "  mov (%%rax), %%rax"
    and ``string-set!`` env si string index value =
        emitExpr env si string
        emitfn "  mov %%rax, %%rcx"
        emitExpr env si index
        emitfn "  sar $%d, %%rax" fixnumShift
        emitfn "  mov %%rax, %%rdx"
        emitExpr env si value
        emitfn "  sar $%d, %%rax" charShift
        emitfn "  movb %%al, %d(%%rcx, %%rdx)" (wordSize - stringTag)
    and ``string-ref`` env si =
        emitfn "  sar $%d, %%rax" fixnumShift
        emitfn "  mov %%rax, %%rdx"
        emitfn "  mov %d(%%rsp), %%rcx" si
        emitfn "  movzb %d(%%rcx, %%rdx), %%rax" (wordSize - stringTag)
        emitfn "  sal $%d, %%rax" charShift
        emitfn "  or $%d, %%rax" charTag
    and ``char=`` env si =
        emitfn "  mov %d(%%rsp), %%rcx" si
        emitfn "  sar $%d, %%rcx" charShift
        emitfn "  sar $%d, %%rax" charShift
        emitfn "  cmp %%al, %%cl"
        emitToBoolEq ()

    and ``fx+`` env si =
        emitfn "  add %d(%%rsp), %%rax" si
    and ``fx-`` env si =
        emitfn "  mov %%rax, %d(%%rsp)" (si - wordSize)
        emitfn "  mov %d(%%rsp), %%rax" si
        emitfn "  sub %d(%%rsp), %%rax" (si - wordSize)
    and ``fx*`` env si =
        emitfn "  imul %d(%%rsp), %%rax" si
        emitfn "  sar $%d, %%rax" fixnumShift
    and ``fxlogor`` env si =
        emitfn "  or %d(%%rsp), %%rax" si
    and ``fxlogand`` env si =
        emitfn "  and %d(%%rsp), %%rax" si
    and ``fx=`` env si =
        emitfn "  cmp %d(%%rsp), %%rax" si
        emitToBoolEq ()
    and ``fx<`` env si =
        emitCompare "g" si
    and ``fx<=`` env si =
        emitCompare "ge" si
    and ``fx>`` env si =
        emitCompare "l" si
    and ``fx>=`` env si =
        emitCompare "le" si
    and cons env si =
        emitfn "  mov %%rax, %d(%%rbp)"  cdrOffset
        emitfn "  mov %d(%%rsp), %%rax" si
        emitfn "  mov %%rax, %d(%%rbp)" carOffset
        emitfn "  mov %%rbp, %%rax"
        emitfn "  or $%d, %%rax" pairTag
        emitfn "  add $%d, %%rbp" (align pairSize)
    and ``set-car!`` env si =
        emitfn "  mov %%rax, %d(%%rsp)" (si - wordSize)
        emitfn "  mov %d(%%rsp), %%rax" si
        emitfn "  sub $%d, %%rax" pairTag
        emitfn "  mov %d(%%rsp), %%rbx" (si - wordSize)
        emitfn "  mov %%rbx, %d(%%rax)" carOffset
    and ``set-cdr!`` env si =
        emitfn "  mov %%rax, %d(%%rsp)" (si - wordSize)
        emitfn "  mov %d(%%rsp), %%rax" si
        emitfn "  sub $%d, %%rax" pairTag
        emitfn "  mov %d(%%rsp), %%rbx" (si - wordSize)
        emitfn "  mov %%rbx, %d(%%rax)" cdrOffset

    and isPrimitive name = List.exists (fun (n, _, _)->n = name) primitives
    and findPrimitive name = List.tryFind (fun (n, _, _) -> name=n) primitives
    and primitives = [ 
        "fxadd1", 1, emitUnary ``fxadd1``
        "add1", 1, emitUnary ``fxadd1``
        "fxsub1", 1, emitUnary ``fxsub1``
        "fixnum->char", 1, emitUnary ``fixnum->char``
        "number->char", 1, emitUnary ``fixnum->char``
        "char->fixnum", 1, emitUnary ``char->fixnum``
        "char->number", 1, emitUnary ``char->fixnum``
        "fixnum?", 1, emitUnary ``fixnum?``
        "number?", 1, emitUnary ``fixnum?``
        "null?", 1, emitUnary ``null?``
        "zero?", 1, emitUnary ``zero?``
        "fxzero?", 1, emitUnary ``zero?``
        "char?", 1, emitUnary ``char?``
        "pair?", 1, emitUnary ``pair?``
        "vector?", 1, emitUnary ``vector?``
        "string?", 1, emitUnary ``string?``
        "boolean?", 1, emitUnary ``boolean?`` 
        "not", 1, emitUnary ``not`` 
        "fxlognot", 1, emitUnary ``fxlognot``
        "car", 1, emitUnary ``car``
        "cdr", 1, emitUnary ``cdr``
        "make-vector", 1, emitUnary ``make-vector``
        "vector-length", 1, emitUnary ``vector-length``
        "make-string", 1, emitUnary ``make-string``
        "string-length", 1, emitUnary ``string-length``
        "fx+", 2, emitBinary ``fx+``
        "fx-", 2, emitBinary ``fx-``
        "fx*", 2, emitBinary ``fx*``
        "fxlogor", 2, emitBinary ``fxlogor``
        "fxlogand", 2, emitBinary ``fxlogand``
        "fx=", 2, emitBinary ``fx=``
        "fx<", 2, emitBinary ``fx<``
        "fx<=", 2, emitBinary ``fx<=``
        "fx>", 2, emitBinary ``fx>``
        "fx>=", 2, emitBinary ``fx>=``
        "eq?", 2, emitBinary ``fx=``
        "cons", 2, emitBinary ``cons``
        "set-car!", 2, emitBinary ``set-car!``
        "set-cdr!", 2, emitBinary ``set-cdr!``
        "vector-ref", 2, emitBinary ``vector-ref``
        "string-ref", 2, emitBinary ``string-ref``
        "char=", 2, emitBinary ``char=``
    ]
    
    emitMainExpr mainExpr

    sb.ToString()