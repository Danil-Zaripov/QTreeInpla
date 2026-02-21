#!/usr/bin/env -S dotnet fsi

#r "nuget: Argu, 6.2.5"

open System.Diagnostics
open System.IO


type ProcessError<'startupError, 'runtimeError> =
    | ProcessStart of 'startupError
    | RuntimeError of 'runtimeError
    | NotSuccessfulExitCode of int

let handleError (filePath, error: ProcessError<System.Exception, System.Exception>) =
    match error with
    | ProcessError.ProcessStart e -> eprintfn "ERROR: %s Could not start Inpla process.\n%s" filePath e.Message
    | ProcessError.RuntimeError e -> eprintfn "ERROR: %s Runtime error.\n%s" filePath e.Message
    | ProcessError.NotSuccessfulExitCode code ->
        eprintfn "ERROR: %s Inpla exited with a non successful exit code %d." filePath code


let startOtherProgram verbose name (args: string) =
    let startInfo = ProcessStartInfo(name)
    startInfo.Arguments <- args
    startInfo.RedirectStandardOutput <- true

    if verbose then
        printfn "INVOKING: %s %s" name args

    try
        let proc = Process.Start(startInfo)
        Ok proc
    with e ->
        Error <| ProcessError.ProcessStart e

let invokeInplaPath verbose inplaPath filePath =
    startOtherProgram verbose inplaPath (sprintf "-f \"%s\"" filePath)

let inplaSuccessfulExitCode = 255

let getStandardOutputAndWait (proc: Process) =
    use proc = proc

    try
        let stdout = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        let exitCode = proc.ExitCode

        if exitCode <> inplaSuccessfulExitCode then
            Error <| ProcessError.NotSuccessfulExitCode exitCode
        else
            Ok stdout
    with e ->
        Error <| ProcessError.RuntimeError e

let getFilesInTestDirectory () =
    System.IO.Directory.GetFiles("./test/", "*.in")

// removes (n interactions, t sec) lines
// and also the first line which is always
// some Inpla information
let removeIrrelevantData (s: string) =
    let lines: string array = s.Split '\n'

    let removed: string array =
        Array.filter (fun x -> not (x.Contains "interactions")) lines[1..]

    String.concat "\n" removed

let handleFile verbose inplaPath filePath =
    let proc = invokeInplaPath verbose inplaPath filePath
    let stdout = Result.bind getStandardOutputAndWait proc
    let removed = Result.map removeIrrelevantData stdout
    (filePath, removed)

let storedDataPath = "./test/golden/"

let getGoldenFilePath (filePath: string) =
    let filename = Path.GetFileNameWithoutExtension(filePath)
    let filenameWithExtension = filename + ".golden"
    let path = Path.Join(storedDataPath, filenameWithExtension)
    path

type CompareResult =
    | Equal
    // line number, (expected, actual)
    | NotEqual of (int * (string * string))
    | FileDoesNotExist

let normalize (s: string) = s.ReplaceLineEndings("\n").TrimEnd()


// Returns: (n, (s2 nth line, s1 nth line)) option
let diffComparison (s1: string) (s2: string) =
    let lines1, lines2 = s1.Split('\n'), s2.Split('\n')
    let len1, len2 = Array.length lines1, Array.length lines2
    let lmin = min len1 len2

    let eqDiff =
        Array.zip lines1[0 .. lmin - 1] lines2[0 .. lmin - 1]
        |> Array.zip [| 1..lmin |]
        |> Array.tryFind (fun (_, (x, y)) -> x <> y)

    let structuralDiff () =
        match (len1, len2) with
        | (len1, len2) when len1 > len2 ->
            let lastLine = lines1.[len2]
            Some(len2, (lastLine, "<EOF>"))
        | (len1, len2) when len1 < len2 ->
            let lastLine = lines2.[len1]
            Some(len1, ("<EOF>", lastLine))
        | _ -> None

    Option.orElseWith structuralDiff eqDiff

let compareResultToGoldenFile goldenPath str =
    if not <| Path.Exists goldenPath then
        CompareResult.FileDoesNotExist
    else
        let fileStr = File.ReadAllText goldenPath

        match diffComparison (normalize fileStr) (normalize str) with
        | Some diff -> CompareResult.NotEqual diff
        | None -> CompareResult.Equal

let compareResult (filePath: string, str: string) =
    let goldenPath = getGoldenFilePath filePath
    let result = compareResultToGoldenFile goldenPath str

    match result with
    | CompareResult.Equal ->
        printfn "OK: Result of %s is equal to %s" filePath goldenPath
        true
    | CompareResult.NotEqual(n, (expected, actual)) ->
        printfn
            "FAILED: File %s\n        difference on line %d\nExpected: %s\n  Actual: %s"
            goldenPath
            n
            expected
            actual

        false
    | CompareResult.FileDoesNotExist ->
        printfn "FAILED: %s does not exist, consider `test.fsx --mode=generate`" goldenPath
        false

let writeResult (filePath: string, str: string) =
    try
        if not (Directory.Exists storedDataPath) then
            Directory.CreateDirectory storedDataPath |> ignore

        let goldenPath = getGoldenFilePath filePath

        match compareResultToGoldenFile goldenPath str with
        | CompareResult.Equal -> printfn "UNCHANGED: %s" goldenPath
        | CompareResult.NotEqual _ ->
            printfn "UPDATED: %s" goldenPath
            File.WriteAllText(goldenPath, str)
        | CompareResult.FileDoesNotExist ->
            printfn "CREATED: %s" goldenPath
            File.WriteAllText(goldenPath, str)

        true
    with e ->
        eprintfn "ERROR: Failed writing result for %s.\n%s" filePath e.Message
        false


open Argu

type ScriptMode =
    | Compare
    | Generate

type CliArguments =
    | [<ExactlyOnce; MainCommand>] Inpla_Path of path: string
    | [<EqualsAssignment>] Mode of ScriptMode
    | [<AltCommandLine("-v")>] Verbose

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Inpla_Path _ -> "specify path to Inpla interpreter executable."
            | Mode _ -> "specify what should this script do (default: compare)."
            | Verbose -> "print additional information."

let main (args: string array) =
    try
        let parser = ArgumentParser.Create<CliArguments>(programName = "test.fsx")
        // first argument is the path to this script
        let argParseResults = parser.Parse args[1..]
        let inplaPath = argParseResults.GetResult Inpla_Path
        let mode = argParseResults.GetResult(Mode, defaultValue = ScriptMode.Compare)
        let verbose = argParseResults.Contains(Verbose)

        let testFiles = getFilesInTestDirectory () |> Array.toList
        let results = List.map (handleFile verbose inplaPath) testFiles

        let oks, errors =
            List.foldBack
                (fun (path, r) (oks, errs) ->
                    match r with
                    | Ok v -> ((path, v) :: oks, errs)
                    | Error e -> (oks, (path, e) :: errs))
                results
                ([], [])

        if not (List.isEmpty errors) then
            errors |> List.iter handleError
            1
        else
            match mode with
            | ScriptMode.Compare ->
                let results = oks |> List.map compareResult

                let passed, total = results |> List.filter id |> List.length, results |> List.length

                printfn "%d passed, %d failed, %d total" passed (total - passed) total

                if passed = total then 0 else 1

            | ScriptMode.Generate ->
                let results = oks |> List.map writeResult

                if List.forall id results then 0 else 1

    with :? ArguParseException as e ->
        eprintfn "%s" e.Message
        1

exit <| main fsi.CommandLineArgs
