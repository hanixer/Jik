open System.IO

#load "references.fsx"

open SExpr
open Core

desugar (stringToSExpr "(or )") |> sexprToString