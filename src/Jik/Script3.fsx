let rec findLowestColor bannedSet =
    Seq.initInfinite id
    |> Seq.find (fun x -> 
        printfn "%d" x
        Seq.exists ((=) x) bannedSet |> not)

let rec findLowestColor2 bannedSet =
    let rec loop n =
        if Seq.exists ((=) n) bannedSet |> not then
            n
        else loop n + 1
    loop 0

let banned = [|0..500|]

let rec findLowestColor3 bannedSet =
    let rec loop n xs =
        printfn "%d" n
        if Seq.isEmpty xs then
            n
        else
            let h = Seq.head xs
            let t = Seq.tail xs
            if n = h then
                loop (n + 1) t
            else n
    loop 0 bannedSet

#time

findLowestColor2 banned