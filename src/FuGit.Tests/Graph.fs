module Graph.Tests

open System.IO
open Expecto
open FuGit.Features.Graph
open LibGit2Sharp

let testDataDirectory = DirectoryInfo(  __SOURCE_DIRECTORY__ + "/../../testdata" |> System.IO.Path.GetFullPath)
let merge1RepositoryPath = Path.Combine(testDataDirectory.FullName, "merge1")

let (|C|_|) (node, _) =
    if node.Commit.IsSome then Some ()
    else None

let (|``P╴``|_|) (node, _) =
    match node.Path.HalfLeft with
    | Some _  -> Some ()
    | _ -> None

let (|``P╶``|_|) (node, _) =
    match node.Path.HalfLeft with
    | Some _  -> Some ()
    | _ -> None

let (|``P╵``|_|) (node, _) =
    match node.Path.HalfTop with
    | Some _  -> Some ()
    | _ -> None

let (|``P╷``|_|) (node, _) =
    match node.Path.HalfBottom with
    | Some _  -> Some ()
    | _ -> None

let (|``P┐``|_|) (node, _) =
    match node.Path.HalfBottom, node.Path.HalfLeft with
    | Some _ , Some _ -> Some ()
    | _ -> None

let (|``P┌``|_|) (node, _) =
    match node.Path.HalfBottom, node.Path.HalfRight with
    | Some _ , Some _ -> Some ()
    | _ -> None

let (|``P┘``|_|) (node, _) =
    match node.Path.HalfTop, node.Path.HalfLeft with
    | Some _ , Some _ -> Some ()
    | _ -> None

let (|``P└``|_|) (node, _) =
    match node.Path.HalfTop, node.Path.HalfRight with
    | Some _ , Some _ -> Some ()
    | _ -> None


let (|``P─``|_|) (node, _) =
    match node.Path.HalfLeft, node.Path.HalfRight with
    | Some _ , Some _ -> Some ()
    | _ -> None

let (|``P│``|_|) (node, _) =
    match node.Path.HalfBottom, node.Path.HalfTop with
    | Some _ , Some _ -> Some ()
    | _ -> None

let (|``E``|_|) (node, _) =
    if node = RepositoryGraph.emptyNode then Some ()
    else None


[<Tests>]
let tests =
    testList
        "Graph"
        [
            testCase "should handle multiple branches"
            <| fun _ ->

                use repository = new LibGit2Sharp.Repository(merge1RepositoryPath)

                let commits = RepositoryGraph.loadRefsAndCommits repository |> List.map snd

                let (nextCommits1, n, _) = RepositoryGraph.computeNextGraphRow commits None

                Expect.isTrue
                    (match n with
                        | [ (C & ``P┌``); ``P┐`` ] -> true
                        | _ -> false)
                    "Row 1 graph doesn't match"


                let (nextCommits2, n2, _) = RepositoryGraph.computeNextGraphRow nextCommits1 (Some n)

                Expect.isTrue
                    (match n2 with
                        | [ ``P│``; (``C`` & ``P│``) ] -> true
                        | _ -> false)
                    "Row 2 graph doesn't match"

                let (nextCommits3, n3, _) = RepositoryGraph.computeNextGraphRow nextCommits2 (Some n2)

                Expect.isTrue
                    (match n3 with
                        | [ ``P│``; ``P│``; ``C`` & ``P╷`` ] -> true
                        | _ -> false)
                    "Row 3 graph doesn't match"

                let (nextCommits4, n4, _) = RepositoryGraph.computeNextGraphRow nextCommits3 (Some n3)

                Expect.isTrue
                    (match n4 with
                        | [ ``C`` & ``P│``; ``P│``; ``P│`` ] -> true
                        | _ -> false)
                    "Row 4 graph doesn't match"

                let (nextCommits5, n5, _) = RepositoryGraph.computeNextGraphRow nextCommits4 (Some n4)

                Expect.isTrue
                    (match n5 with
                        | [ ``C`` & ``P└``; ``P┘`` & ``P─``; ``P┘`` ] -> true
                        | _ -> false)
                    $"Row 5 graph doesn't match expect C (┘ & ─) ┘ got {n5}"

                let (nextCommits6, n6, _) = RepositoryGraph.computeNextGraphRow nextCommits5 (Some n5)
                Expect.isEmpty n6 "Row 6 doesn't match"

        ]