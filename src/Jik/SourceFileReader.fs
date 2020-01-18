module SourceFileReader

open System.Text
open System.IO

let combine baseF path = System.IO.Path.Combine([| baseF; path |])

let rec expandModules baseF (pathNames : seq<string>) =
    [ for (path : System.String) in pathNames do
          let path = combine baseF path
          if path.EndsWith(".mscm") then
              let parent = Directory.GetParent(path).FullName
              let children = System.IO.File.ReadAllLines(path)
              let children' =
                  Seq.filter
                      (fun (ch : string) ->
                      ch.Length > 0 && not (ch.StartsWith(";"))) children
              yield! (expandModules parent children')
          else yield path ]
    |> List.distinct

let readFiles paths =
    let sb = StringBuilder()
    for path in paths do
        sb.Append(File.ReadAllText(path)) |> ignore
    sb.ToString()

let readFilesExpandingModules pathNames =
    let paths = expandModules "" pathNames
    readFiles paths
