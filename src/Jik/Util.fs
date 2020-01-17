module Util

type ProcessResult = { exitCode : int; stdout : string; stderr : string }
let executeProcess (exe,cmdline) =
    let psi = System.Diagnostics.ProcessStartInfo(exe,cmdline)
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.CreateNoWindow <- true
    let p = System.Diagnostics.Process.Start(psi)
    let output = System.Text.StringBuilder()
    let error = System.Text.StringBuilder()
    p.OutputDataReceived.Add(fun args -> output.Append(args.Data + "\n") |> ignore)
    p.ErrorDataReceived.Add(fun args -> error.Append(args.Data + "\n") |> ignore)
    p.BeginErrorReadLine()
    p.BeginOutputReadLine()
    p.WaitForExit()
    { exitCode = p.ExitCode; stdout = output.ToString(); stderr = error.ToString() }

let zipp def a b =
    let rec loop acc a b =
        if Seq.isEmpty a |> not && Seq.isEmpty b |> not then
            let acc = (Seq.head a, Seq.head b) :: acc
            loop acc (Seq.tail a) (Seq.tail b)
        elif Seq.isEmpty a |> not then
            let acc = (Seq.head a, def) :: acc
            loop acc (Seq.tail a) b
        elif Seq.isEmpty b |> not then
            let acc = (def, Seq.head b) :: acc
            loop acc a (Seq.tail b)
        else acc
    loop [] a b
    |> List.rev

let appendStringsByCol (str1 : string) (str2 : string) =
    let strs1 = str1.Split('\n')
    let strs2 = str2.Split('\n')
    let maxColumn = (Array.maxBy Seq.length strs1 |> Seq.length) + 4
    zipp "" strs1 strs2
    |> Seq.map (fun (s1, s2) ->
        s1.PadRight(maxColumn) + s2)
    |> String.concat "\n"

let getPathRelativeToRoot path =
    let sd = __SOURCE_DIRECTORY__
    let p = sd + "\\..\\..\\" + path
    let p = System.IO.Path.GetFullPath p
    p