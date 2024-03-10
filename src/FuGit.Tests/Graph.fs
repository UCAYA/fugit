module Graph.Tests

open System.IO
open Expecto
open FuGit.Features.Graph
open LibGit2Sharp

let testDataDirectory = DirectoryInfo(  __SOURCE_DIRECTORY__ + "/../../testdata" |> System.IO.Path.GetFullPath)
let merge1RepositoryPath = Path.Combine(testDataDirectory.FullName, "merge1")

let (|C|_|) (node:GraphNode) =
    if node.Commit.IsSome then Some ()
    else None

let (|``P╴``|_|) (node:GraphNode) =
    node.Shapes
    |> List.tryPick (function
        | GraphShape.HorizontalLeft _ -> Some ()
        | _ -> None)

let (|``P╶``|_|) (node:GraphNode) =
    node.Shapes
    |> List.tryPick (function
        | GraphShape.HorizontalRight _ -> Some ()
        | _ -> None)

let (|``P╵``|_|) (node:GraphNode) =
    node.Shapes
    |> List.tryPick (function
        | GraphShape.End -> Some ()
        | _ -> None
    )

let (|``P╷``|_|) (node:GraphNode) =
    node.Shapes
    |> List.tryPick (function
        | GraphShape.Start -> Some ()
        | _ -> None
    )

let (|``P┐``|_|) (node:GraphNode) =
    node.Shapes
    |> List.tryPick (function
        | GraphShape.HorizontalRightToBottom _ -> Some ()
        | _ -> None)

let (|``P┌``|_|) (node:GraphNode) =
    node.Shapes
    |> List.tryPick (function
        | GraphShape.HorizontalLeftToBottom _ -> Some ()
        | _ -> None)

let (|``P┘``|_|) (node:GraphNode) =
    node.Shapes
    |> List.tryPick (function
        | GraphShape.VerticalTopToLeft _ -> Some ()
        | _ -> None)

let (|``P└``|_|) (node:GraphNode) =
    node.Shapes
    |> List.tryPick (function
        | GraphShape.VerticalTopToRight _ -> Some ()
        | _ -> None)

let (|``P─``|_|) (node:GraphNode) =
    node.Shapes
    |> List.tryPick (function
        | GraphShape.Horizontal _ -> Some ()
        | _ -> None)

let (|``P│``|_|) (node:GraphNode) =
    node.Shapes
    |> List.tryPick (function
        | GraphShape.Vertical -> Some ()
        | _ -> None)

[<Tests>]
let tests =
    testList
        "Graph"
        [
            testCase "should handle multiple branches"
            <| fun _ ->

                use repository = new LibGit2Sharp.Repository(merge1RepositoryPath)

                let commits = RepositoryGraph.loadRefsAndCommits repository |> List.map snd

                let (nextCommits1, row, _) = RepositoryGraph.computeNextGraphRow commits None

                Expect.isTrue
                    (match row.Cells with
                        | [ Some (C & ``P╷`` & ``P╶``); Some ``P┐`` ] -> true
                        | _ -> false)
                    "Row 1 graph doesn't match"


                let (nextCommits2, row2, _) = RepositoryGraph.computeNextGraphRow nextCommits1 (Some row)

                Expect.isTrue
                    (match row2.Cells with
                        | [ Some ``P│``; Some (``C`` & ``P│``) ] -> true
                        | _ -> false)
                    "Row 2 graph doesn't match"

                let (nextCommits3, row3, _) = RepositoryGraph.computeNextGraphRow nextCommits2 (Some row2)

                Expect.isTrue
                    (match row3.Cells with
                        | [ Some ``P│``; Some ``P│``; Some ``C`` & Some ``P╷`` ] -> true
                        | _ -> false)
                    "Row 3 graph doesn't match"

                let (nextCommits4, row4, _) = RepositoryGraph.computeNextGraphRow nextCommits3 (Some row3)

                Expect.isTrue
                    (match row4.Cells with
                        | [ Some ``C`` & Some ``P│``; Some ``P│``; Some ``P│`` ] -> true
                        | _ -> false)
                    "Row 4 graph doesn't match"

                let (nextCommits5, row5, _) = RepositoryGraph.computeNextGraphRow nextCommits4 (Some row4)

                Expect.isTrue
                    (match row5.Cells with
                        | [ Some (``C`` & ``P╵`` & ``P╶``); Some (``P┘`` & ``P─``); Some ``P┘`` ] -> true
                        | _ -> false)
                    $"Row 5 graph doesn't match expect C (┘ & ─) ┘ got {row5}"

                let (nextCommits6, row6, _) = RepositoryGraph.computeNextGraphRow nextCommits5 (Some row5)
                Expect.isEmpty row6.Cells "Row 6 doesn't match"

        ]