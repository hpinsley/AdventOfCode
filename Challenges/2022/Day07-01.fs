module Year2022Day07_Part1

open System
open System.IO
open Common
open System.Text.RegularExpressions

type DataFile = {
    Filename: string;
    Size: int
}

type Folder = {
    Name: string;
    mutable Files: DataFile list;
    mutable Subfolders: Folder list;
    Parent: Folder option;
}

type Context = {
    Root: Folder;
    CurrentFolder: Folder;
}

type Instruction =
    | DIR of string
    | CD of string
    | LS
    | File of int * string

let getInstruction (line:string): Instruction =
    match line with
        | ParseRegex "^\$ cd (.*)" [folder] -> CD folder
        | ParseRegex "^\$ ls" [] -> LS
        | ParseRegex "^dir (.*)" [folder] -> DIR folder
        | ParseRegex "^([0-9]+) (.*)" [size; filename] -> File (int size, filename)

let rec getFolderSize (folder:Folder) : int =
    let fileSizeTotal = folder.Files |> List.sumBy (fun f -> f.Size)
    let subfoldersTotalSize = folder.Subfolders |> List.map getFolderSize |> List.sum
    fileSizeTotal + subfoldersTotalSize

let ApplyInstruction (context:Context) (instruction:Instruction) : Context =

    printfn "Applying: %A to %A" instruction "(context)"

    try
        match instruction with
            | CD folder ->
                match folder with
                    | "/" -> { context with CurrentFolder = context.Root }
                    | ".." -> match context.CurrentFolder.Parent with
                                | Some parent -> { context with CurrentFolder = parent }
                                | None -> failwith "Can't go up from root"
                    | subfolderName -> 
                        let nextFolder = context.CurrentFolder.Subfolders |> List.tryFind (fun sf -> sf.Name = subfolderName)
                        match nextFolder with
                            | Some f -> { context with CurrentFolder = f }
                            | None -> sprintf "Cannot find folder %s" subfolderName |> failwith

            | DIR folder ->
                let newFolder = { Name = folder; Files = []; Subfolders = []; Parent = Some context.CurrentFolder }
                context.CurrentFolder.Subfolders <- newFolder :: context.CurrentFolder.Subfolders
                context
                
            | LS _ -> context
            | File (filesize, filename) -> 
                context.CurrentFolder.Files <- { Filename = filename; Size = filesize } :: context.CurrentFolder.Files
                context

    with ex  ->
        printfn "error: %A" ex
        failwith "Error"

let rec printFolder (folder:Folder) (indent:int) : unit =
    let folderSize = getFolderSize folder
    let padding = String.replicate indent " "
    printfn "%s Folder %s (%d)" padding folder.Name folderSize
    for f in folder.Files do
        printfn " %s File: %s (%d)" padding f.Filename f.Size
    for sf in folder.Subfolders do
        printFolder sf (indent + 4)

let rec getFolderList (folder:Folder) : Folder list  =
    let subLists = folder.Subfolders |> List.map getFolderList
    let flattened: Folder list = List.concat subLists
    folder :: flattened

let solve =

    // let lines = Common.getSampleDataAsArray 2022 07
    let lines = Common.getChallengeDataAsArray 2022 07
    // printfn "%A" lines

    let instructions = lines |> Array.map getInstruction

    //let paired = Array.zip lines instructions
    //printfn "%A" paired

    //for instruction in instructions do
    //    printfn "%A" instruction

    let root = { Name = "/"; Files = []; Subfolders = []; Parent = None }
    let context = { Root = root; CurrentFolder = root }


    let reduction = instructions
                        |> Array.fold ApplyInstruction context

    let tree = reduction.Root
    printFolder tree 0

    let folderList = getFolderList tree
    
    for f in folderList do
        printfn "Folder %s with total size %d" f.Name (getFolderSize f)

    let elible = folderList |> List.filter (fun f -> (getFolderSize f) <= 100000)
    let answer = elible |> List.sumBy (fun f -> getFolderSize f)
    printfn "Part 1: %d" answer
    

    let volSize = 70000000
    let used = getFolderSize root
    let requiredFree = 30000000
    let currentFree = volSize - used
    let minToFree = requiredFree - currentFree
    printfn "Need to free up %d" minToFree

    let folderToFree = folderList
                            |> List.filter (fun f -> (getFolderSize f) >= minToFree)
                            |> List.minBy (fun f -> getFolderSize f)
    printfn "Delete folder %s with size %d" folderToFree.Name (getFolderSize folderToFree)

