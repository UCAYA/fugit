module Graph.Tests

open System.IO
open Expecto
open FuGit.Graph
open LibGit2Sharp

let testDataDirectory = DirectoryInfo(  __SOURCE_DIRECTORY__ + "/../../testdata" |> System.IO.Path.GetFullPath)
let merge1RepositoryPath = Path.Combine(testDataDirectory.FullName, "merge1")

let (|C|_|) nodeType =
    match nodeType with
    | Commit (_node, _columnIndex) -> Some ()
    | _ -> None

let (|``P┐``|_|) (nodeType) =
    match nodeType with
    | Path (p, _) when p |> List.exists (function | PathNode.HorizontalToBottom _ -> true | _ -> false) -> Some ()
    | _ -> None

let (|``P┌``|_|) nodeType =
    match nodeType with
    | Path (p, _) when p |> List.exists (function | PathNode.HorizontalToBottom _ -> true | _ -> false) -> Some ()
    | _ -> None

let (|``P┘``|_|) nodeType =
    match nodeType with
    | Path (p, _) when p |> List.exists (function | PathNode.TopToHorizontal _ -> true | _ -> false) -> Some ()
    | _ -> None

let (|``P└``|_|) nodeType =
    match nodeType with
    | Path (p, _) when p |> List.exists (function | PathNode.TopToHorizontal _ -> true | _ -> false) -> Some ()
    | _ -> None

let (|``P─``|_|) nodeType =
    match nodeType with
    | Path (p, _) when p |> List.exists (function | PathNode.Horizontal -> true | _ -> false) -> Some ()
    | _ -> None

let (|``P│``|_|) nodeType =
    match nodeType with
    | Path (p, _) when p |> List.exists (function | PathNode.Vertical _ -> true | _ -> false) -> Some ()
    | _ -> None

let (|``E``|_|) nodeType =
    match nodeType with
    | Empty _ -> Some ()
    | _ -> None

[<Tests>]
let tests =
    testList
        "Graph"
        [
            testCase "should handle multiple branches"
            <| fun _ ->

                use repository = new LibGit2Sharp.Repository(merge1RepositoryPath)

                let refsAndCommits = RepositoryGraph.loadRefsAndCommits repository

                let (nRefsAndCommits, n)  = RepositoryGraph.walkCommitAndRefsAndCreateParentsTree refsAndCommits None
                Expect.isTrue (
                    match n with
                    | [ C; ``P┐`` ] -> true
                    | _ -> false
                ) "Row 1 graph doesn't match"


                let (nRefsAndCommits2,n2)  = RepositoryGraph.walkCommitAndRefsAndCreateParentsTree nRefsAndCommits (Some n)
                Expect.isTrue (
                    match n2 with
                    | [ ``P│``; ``C`` ] -> true
                    | _ -> false
                ) "Row 2 graph doesn't match"

                let (nRefsAndCommits3,n3)  = RepositoryGraph.walkCommitAndRefsAndCreateParentsTree nRefsAndCommits2 (Some n2)
                Expect.isTrue (
                    match n3 with
                    | [ ``P│``; ``P│``; ``C`` ] -> true
                    | _ -> false
                ) "Row 3 graph doesn't match"

                let (nRefsAndCommits4,n4)  = RepositoryGraph.walkCommitAndRefsAndCreateParentsTree nRefsAndCommits3 (Some n3)
                Expect.isTrue (
                    match n4 with
                    | [ ``C``; ``P│``; ``P│`` ] -> true
                    | _ -> false
                ) "Row 4 graph doesn't match"

                let (nRefsAndCommits5,n5)  = RepositoryGraph.walkCommitAndRefsAndCreateParentsTree nRefsAndCommits4 (Some n4)
                Expect.isTrue (
                    match n5 with
                    | [ ``C``; ( ``P┘`` | ``P─``); ``P┘`` ] -> true
                    | _ -> false
                ) "Row 5 graph doesn't match"

                let (nRefsAndCommits6,n6)  = RepositoryGraph.walkCommitAndRefsAndCreateParentsTree nRefsAndCommits4 (Some n5)
                Expect.isEmpty n5 "Row 6 doesn't match"

        ]