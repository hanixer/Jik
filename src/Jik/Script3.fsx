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

let s1 = "something
go
there
now
yo
can 
see 
what is exactly is the
lopwerst part in the
control flow"
let s2 = "1
2
3
4
5
6
7
8
6
54
6
23
2
3
4
5
"
let s3 = "
!
@
#
$
%
^
&
*
&
^
&
^
&
*
&
^
%
^
"

appendStringsByCol s1 s2 |> appendStringsByCol <| s3 |>  printfn "%s"