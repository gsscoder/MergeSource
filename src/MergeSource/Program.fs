module MergeSources

open System.IO
open System
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq
open CommandLine
open Chessie.ErrorHandling

type options = {
    [<Option('p', "project", Required = true, HelpText = "Project with sources to merge.")>]
    ProjectName : string
    [<Option('d', "destdir", HelpText = "Destination directory of merged file.")>]
    DestinationDir : string
}

let inline (|Success|Fail|) (result : ParserResult<'a>) =
    match result with
    | :? Parsed<'a> as parsed -> Success(parsed.Value)
    | :? NotParsed<'a> as notParsed -> Fail(notParsed.Errors)
    | _ -> failwith "invalid parser result"

let inline contains (str : string, what) = str.IndexOf(what, 0, StringComparison.CurrentCultureIgnoreCase) >= 0

let inline startswith (str : string, what) = str.TrimStart([|' '|]).StartsWith(what, StringComparison.CurrentCultureIgnoreCase)

let regex s = new Regex(s, RegexOptions.Compiled)
let attribRe = regex @"(\S+)=[""']?((?:.(?![""']?\s+(?:\S+)=|[>""']))+.)[""']?"

let parseProject file =
    let parseInclude input =
        let r = attribRe.Match(input)
        if r.Success && r.Groups.Count = 3
        then Some r.Groups.[2].Value
        else None
    let path = file |> Path.GetFullPath |> Path.GetDirectoryName
    File.ReadAllLines file
    |> PSeq.ofArray
    |> PSeq.filter (fun l -> contains(l, "Compile"))
    |> PSeq.filter (fun l -> contains(l, "Include"))
    |> PSeq.map parseInclude
    |> PSeq.filter (function Some(_) -> true | None -> false)
    |> PSeq.map Option.get
    |> PSeq.filter (fun l -> not(contains(l, "AssemblyInfo")))
    |> PSeq.map (fun l -> Path.Combine(path,l))

type Namespace = Namespace of string
type Using = Using of string
    with
        override this.ToString() =
            match this with
            | Using(x) -> x
type Define = Define of string
    with
        override this.ToString() =
            match this with
            | Define(x) -> x
type FileName = FileName of string
type Source =
    | Info of Namespace * Using list * Define list * FileName
    | Fail

module Array = 
    let inline tryNth i arr =
        if i >= 0  && i <= ((Array.length arr) - 1)
        then Some(Array.get arr i)
        else None
    let inline nth i arr = Array.get arr i

module PSeq =
    let inline tryNth i pseq =
        if i >= 0  && i <= ((PSeq.length pseq) - 1)
        then Some(PSeq.nth i pseq)
        else None

let inline split (s : string) = s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

let inline validUsingStmt (s : string) =
    String.forall (fun c -> Char.IsLetter(c) || Char.IsWhiteSpace(c) || c = '=' || c = '.' || c = ';') s 

let sourceInfo file =
    let inline parseSecond line =
        line |> split |> Array.tryNth 1
    let inline parseSecond' (line : string option) =
        match line with
        | Some(line) -> line |> parseSecond
        | _ -> None
    let parseStmts pred ctor lines =
        lines
        |> PSeq.filter pred
        |> PSeq.map ctor
        |> PSeq.toList
    let parseStmts' pred pred2 ctor lines =
        lines
        |> PSeq.filter pred
        |> PSeq.filter pred2
        |> PSeq.map ctor
        |> PSeq.toList
    let lines = file
                |> File.ReadAllLines
                |> PSeq.ofArray
    let nspace = lines
                 |> PSeq.filter (fun l -> startswith(l, "namespace"))
                 |> PSeq.tryNth 0
                 |> parseSecond'
    match nspace with
    | Some(nspace) ->
        let usings = parseStmts' (fun l -> startswith(l, "using ")) validUsingStmt Using lines
        let defines = parseStmts (fun l -> startswith(l, "#define "))  Define lines
        let defines' = parseStmts (fun l -> startswith(l, "//#define "))  Define lines
        Info(Namespace(nspace), usings, defines |> List.append defines', FileName(file))
    | _ -> Fail

let merge projectFile =
    let readWithoutUsingAndDefine file =
        File.ReadAllLines file
        |> PSeq.ofArray
        |> PSeq.filter (fun l -> not((startswith(l, "using ") && validUsingStmt l) || startswith(l, "#define ") || startswith(l, "//#define ")))
    let files = parseProject projectFile
    let sourceInfos =
        files
        |> PSeq.map (fun s -> sourceInfo s)
        |> PSeq.sort
    let unique pred info =
        info
        |> PSeq.map pred
        |> PSeq.concat
        |> PSeq.distinct
        |> PSeq.sort
    let usings = sourceInfos
                 |> unique (fun s ->
                            match s with
                            | Info(_, usings, _, _) -> usings |> PSeq.ofList
                            | _ -> PSeq.empty)
                 |> PSeq.map (fun x -> x.ToString())
    let defines = sourceInfos 
                  |> unique (fun s ->
                             match s with
                             | Info(_, _, defines, _) -> defines |> PSeq.ofList
                             | _ -> PSeq.empty)
                  |> PSeq.map (fun x -> x.ToString())
    files
    |> PSeq.map readWithoutUsingAndDefine
    |> PSeq.concat
    |> PSeq.append usings
    |> PSeq.append defines

let mergeToFile projectFile destFile =
    let newLines = merge projectFile |> PSeq.toArray
    Result<int, exn>.Try (fun _ ->
                          File.WriteAllLines(destFile, newLines) |> ignore
                          0)

[<EntryPoint>]
let main argv = 
    match Parser.Default.ParseArguments<options>(argv) with
    | Success(opts) ->
        let destFile = if opts.DestinationDir.Length = 0
                       then Path.ChangeExtension(
                                Path.Combine(
                                    Path.GetDirectoryName(opts.ProjectName),
                                    Path.GetFileNameWithoutExtension(opts.ProjectName)), "cs")
                       else opts.DestinationDir
        let result = mergeToFile opts.ProjectName destFile
        Trial.returnOrFail result
    | _ -> 1
