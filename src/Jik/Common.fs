module Common
open System.Collections.Generic

let freshLabel =
    let mutable dict = Dictionary<string, int>()
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