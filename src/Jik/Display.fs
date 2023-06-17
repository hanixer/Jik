module Display

type Iseq = 
    | INil
    | IStr of string
    | IAppend of Iseq * Iseq
    | IIdent of Iseq
    | INewline


let iNil = INil
let iAppend a b = 
    match a, b with
    | INil, _ -> b
    | _, INil -> a
    | _ -> IAppend (a,b)
let iStr = IStr
let iNewline = INewline
let iIndent iseq = IIdent iseq
let iConcat xs = 
    xs |> List.fold iAppend INil
let rec iInterleave sep = function
    | [] -> INil
    | [iseq] -> iseq
    | iseq :: rest -> iseq |>iAppend<| sep |>iAppend<| (iInterleave sep rest)
let iNum n = sprintf "%d" n |> iStr    
let rec flatten col xs =
    let space (n:int) : string = String.replicate n " "

    let rec go (acc : System.Text.StringBuilder) col lkj =
        match lkj with
        | [] -> acc
        | (INewline, indent) :: iseqs ->
            go (acc.Append("\n" + (space indent))) indent iseqs
        | (IIdent iseq, indent) :: iseqs ->
            go acc col ((iseq, col) :: iseqs)
        | (INil, indent) :: iseqs ->  go acc col iseqs
        | (IStr s, indent) :: iseqs -> 
            if s.Contains("\n") then
                let splitted = 
                    s.Split('\n') 
                    |> List.ofArray 
                    |> List.map iStr 
                    |> iInterleave INewline
                go acc col ((splitted, indent) :: iseqs)
            else
                go (acc.Append(s)) (col + s.Length) iseqs
        | (IAppend (iseq1, iseq2), indent) :: iseqs -> 
            go acc col ((iseq1, indent) :: (iseq2, indent) :: iseqs)
    let sb = go (System.Text.StringBuilder()) col xs
    sb.ToString()
let iDisplay seq = flatten 0 [seq, 0]

let iSpace iseqs = iInterleave (iStr " ") iseqs