module Year2018Day08

open System
open System.IO
open Common

exception BadInputException of String

type Node = {
    children: Node list;
    metadata: int list
}

let rec computeMetadataSum node =
    List.sum node.metadata
        + List.sumBy computeMetadataSum node.children

let rec buildNode (vals: int list) : (Node * int list) =

    let buildChildren numChildren vals =
        [1..numChildren]
            |> List.fold
                (
                    fun (children, remainingVals) i ->
                        let (child, remaining) = buildNode remainingVals
                        (List.append children [child], remaining)
                )
                ([], vals)

    match vals with
        | numChildren :: metadataCount :: rest ->
           let (children, afterChildren) = buildChildren numChildren rest
           let (metadata, afterMeta) = List.splitAt metadataCount afterChildren

           let newNode = { children = children; metadata = metadata }
           (newNode, afterMeta)
        | _ ->
           raise (BadInputException ("Seems to be invalid"))

let solvePartOne (tree: Node) =
    // dump "tree" tree
    printfn "\nThe metadata sum is %d" (computeMetadataSum tree)
    ()

let rec nodeValue (node: Node) : int =
    match node.children with
        | [] ->
            List.sum node.metadata
        | _ ->
            let vChildren = Array.ofList node.children
            node.metadata
                |> List.sumBy (fun oneBasedIndex ->
                                let index = oneBasedIndex - 1
                                if (index >= 0 && index < vChildren.Length)
                                then
                                    nodeValue vChildren.[index]
                                else
                                    0)

let solvePartTwo (tree: Node) =
    printfn "part two is %d" <| nodeValue tree

let rec printTree (indent: int) (node: Node) =
    let prefix = String.replicate indent " "
    let nodeText = sprintf "%A (score %d)" node.metadata (nodeValue node)
    printfn "%s%s" prefix nodeText
    List.iter (printTree (indent + 4)) node.children

let solve =
    let testdata = Common.getChallengeData 2018 8
    //let testdata = Common.getSampleData 2018 8
    // dump "data" testdata


    let vals = testdata.Split (' ')
                |> List.ofArray
                |> List.map int

    // dump "Values:" vals

    let (tree, extra) = buildNode(vals)

    solvePartOne tree
    printTree 0 tree

    solvePartTwo tree

    ()
