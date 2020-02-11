module Common
open System.Collections.Generic

type Constant =
    | StringConst of string
    | FloatConst of float

let private dict = Dictionary<string, int>()
let freshLabel =
    fun prefix ->
        if dict.ContainsKey prefix |> not then
            dict.Add(prefix, 0)
        let count = dict.Item prefix
        dict.Item prefix <- count + 1
        if count = 0 then
            prefix
        else
            sprintf "%s%d" prefix count

let freshCodeLabel prefix = freshLabel (prefix + "/code")

/// Ugly... Please rework...
let resetFreshLabels() =
    dict.Clear()