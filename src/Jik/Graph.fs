module Graph

open System.Collections.Generic

type Graph = Dictionary<string, Set<string>>

let makeGraph vertices =
    let graph = Graph()
    Seq.iter (fun vert -> graph.Add(vert, Set.empty)) vertices
    graph

let addEdge (graph : Graph) u v =
    graph.Item u <- graph.Item u |> Set.add v
    graph.Item v <- graph.Item v |> Set.add u

let adjacent (graph : Graph) u = 
    graph.Item u

let vertices (graph : Graph) =
    graph.Keys

let printDot (graph : Graph) fileName = 
    use out = new System.IO.StreamWriter(path=fileName)    
    fprintfn out "strict graph {"
    Seq.iter (fun v ->
        fprintfn out "%s" v) (vertices graph)
    Seq.iter (fun v ->
        Seq.iter (fun u ->
            fprintfn out "%s -- %s" u v) (adjacent graph v)) (vertices graph)
    fprintfn out "}\n"