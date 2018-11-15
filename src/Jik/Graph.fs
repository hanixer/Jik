module Graph

open System.Collections.Generic

type Graph<'T when 'T : comparison> = G of Dictionary<'T, Set<'T>>

let makeGraph vertices =
    let graph = Dictionary()
    Seq.iter (fun vert -> graph.Add(vert, Set.empty)) vertices
    G graph

let addEdge (G graph) u v =
    graph.Item u <- graph.Item u |> Set.add v
    graph.Item v <- graph.Item v |> Set.add u

let adjacent (G graph) u = 
    graph.Item u

let vertices (G graph) =
    graph.Keys

let printDot (G graph as g) fileName = 
    use out = new System.IO.StreamWriter(path=fileName)    
    fprintfn out "strict graph {"
    fprintfn out "layout=circo"
    Seq.iter (fun v ->
        fprintfn out "%s" v) (vertices g)
    Seq.iter (fun v ->
        Seq.iter (fun u ->
            fprintfn out "%s -- %s" u v) (adjacent g v)) (vertices g)
    fprintfn out "}\n"