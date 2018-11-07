#load "Base.fs"
#load "Graph.fs"
#load "Util.fs"
#load "Display.fs"
#load "TestDriver.fs"
#load "RuntimeConstants.fs"
#load "Base.fs"
#load "Core.fs"
#load "Cps.fs"
#load "Codegen.fs"

open Base
open Core
open Cps
open Codegen
open Graph
open TestDriver

type Simple =
    | Prim of Prim * Var list
    | Int of int
    | Bool of bool

and Transfer =
    | Return of Var
    | Jump of Var * Var list
    | Call of Var * Var * Var list
    | If of Var * Var * Var

and Decl = Var * Expr

and Block = Var * Decl list * Transfer

and Function = Var list * Block list * Var

type Term =
    | PrimCall of Prim * Var list * Var
    | Int of int * Var
    | Bool of bool * Var
    | Call of Var * Var list * Var
    | If of Var * Var * Var
    | Return of Var

type Cont = Var * Var list * Term

let rec convert expr next =
    match expr with
    | Expr.If(cond, thn, els) ->
        let thisLabel = freshLabel "ifk"
        let thns, thnk = convert thn next
        let elss, elsk = convert els next
        match cond with
        | Expr.Ref var ->
            thns @ elss @ [thisLabel, If(var, thnk, elsk)], thisLabel
        | _ ->
            let condVar = freshLabel "cond"
            let conds, condk = convert cond thisLabel
            conds @ thns @ elss @ [thisLabel, If(condVar, thnk, elsk)], condk

    | Expr.PrimApp(op, args) ->
        let thisLabel = freshLabel "pa"
        let conts, firstLabel = 
            List.foldBack (fun arg (conts, next) ->
                let argConts, argLabel = convert arg next
                argConts @ conts, argLabel) args ([], thisLabel)
        conts @ [thisLabel, PrimCall(op, )], firstLabel