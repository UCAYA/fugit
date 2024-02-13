namespace FuGit.Features.Graph

open LibGit2Sharp

// r.Head
type Node =
    {
        Reference: Reference
        Commit: Commit
        Parent: Commit option
    }

// type PathId = PathId of int

[<RequireQualifiedAccess>]
type GraphNodeReference = | TargetColumnIndex of int

// type VerticalPath =
//     | Vertical
//     | HalfVerticalTop
//     | HalfVerticalBottom

// type Path =
//     | Vertical of VerticalPath * GraphNodeReference
//     | HorizontalRightToBottom of GraphNodeReference
//     | HorizontalLeftToBottom of GraphNodeReference
//     | TopToHorizontalRight of GraphNodeReference
//     | TopToHorizontalLeft of GraphNodeReference
//     | Horizontal of GraphNodeReference
//     | HalfHorizontalRight of GraphNodeReference
//     | HalfHorizontalLeft of GraphNodeReference

// type NodeType =
//     | Commit of Commit * Reference list * int
//     | Empty of int
//     | Path of PathNode list * int

type GraphNodePath =
    {
        HalfTop: GraphNodeReference option
        HalfRight: GraphNodeReference option
        HalfBottom: GraphNodeReference option
        HalfLeft: GraphNodeReference option
    }

type GraphNode =
    {
        Commit: Commit option
        Path: GraphNodePath
    }
// [<RequireQualifiedAccess>]
// type NodeDrawingType =
//     | ``┌``
//     | ``└``
//     | ``┐``
//     | ``┘``
//     | ``│``
//     | ``─``

module RepositoryGraph =
    let emptyNode =
        {
            Commit = None
            Path =
                {
                    HalfTop = None
                    HalfRight = None
                    HalfBottom = None
                    HalfLeft = None
                }
        }

    let loadRefsAndCommits (repository: Repository) =
        let refAndCommit (ref: Reference) =
            ref, (ref.ResolveToDirectReference().Target |> unbox<Commit>)

        [
            if not (repository.Head.CanonicalName.StartsWith("refs")) then
                repository.Head.Reference |> refAndCommit


            yield! repository.Refs |> Seq.filter (fun ref -> not ref.IsTag) |> Seq.map refAndCommit
        ]
        |> List.sortByDescending (fun (_r, commit) -> commit.Committer.When)


    let updateAtFn index map list =
        let value = list |> List.item index
        list |> List.updateAt index (map value)

    let mapIf condition map value =
        if condition then
            map value
        else
            value

    let insertAtFreeIndex map (list: (GraphNode * Commit option) list) =
        match list |> List.tryFindIndex (fun (_, commit) -> commit.IsNone) with
        | Some idx -> list |> List.updateAt idx (map idx), idx
        | None -> list |> List.insertAt list.Length (map list.Length), list.Length

    let computeNextGraphRow (commits: Commit list) (previousNodesRow: (GraphNode * Commit option) list option) =

        // Prepare a new row, if we have a graph with a path on previous row, we will have a half top vertical path on current row
        let currentRow =
            previousNodesRow
            |> Option.defaultValue List.empty
            |> List.map (fun (graphNode, commit) ->
                match graphNode.Path.HalfBottom with
                | Some target ->
                    { emptyNode with
                        Path.HalfTop = Some target
                        Path.HalfBottom = Some target
                    },
                    commit
                | None -> emptyNode, None
            )
            |> List.rev
            |> List.skipWhile (fun (node, followCommit) -> followCommit.IsNone && node = emptyNode)
            |> List.rev

        match
            commits
            |> List.distinctBy (fun commit -> commit.Id)
            |> List.sortByDescending (fun commit -> commit.Committer.When)
        with
        | [] -> [], currentRow, None
        | (currentCommit) :: nextCommits ->


            let tryFindChildNodeColumnIndexOfCommit c =
                currentRow
                |> List.tryFindIndex (fun (_, nextParentCommit) -> nextParentCommit = Some c)

            let childNodeIndexesOfCurrentCommit =
                currentRow
                |> List.indexed
                |> List.choose (fun (index, (_, nextParentCommit)) ->
                    if nextParentCommit = Some currentCommit then
                        Some index
                    else
                        None
                )

            let splitGraphIndexes = childNodeIndexesOfCurrentCommit |> List.rev |> List.pairwise

            let currentRow, currentNodeIndex =
                let firstCommitParent = currentCommit.Parents |> Seq.tryHead

                match childNodeIndexesOfCurrentCommit |> Seq.tryHead with
                | None ->
                    currentRow
                    |> insertAtFreeIndex (fun idx ->
                        ({ emptyNode with
                            Commit = Some currentCommit
                            Path.HalfBottom =
                                if firstCommitParent.IsSome then
                                    Some(GraphNodeReference.TargetColumnIndex idx)
                                else
                                    None
                         },
                         firstCommitParent)
                    )
                | Some index ->
                    currentRow
                    |> updateAtFn
                        index
                        (fun (node, _commit) ->
                            ({ node with
                                Commit = Some currentCommit
                                Path.HalfBottom =
                                    if firstCommitParent.IsSome then
                                        Some(GraphNodeReference.TargetColumnIndex index)
                                    else
                                        None
                             },
                             firstCommitParent)
                        ),
                    index

            // Split nodes implies that child nodes are already existing in the previous row
            let currentRow =
                (currentRow, splitGraphIndexes)
                ||> List.fold (fun currentRow (endIndex, startIndex) ->
                    (currentRow, [ startIndex..endIndex ])
                    ||> List.fold (fun currentRow index ->
                        currentRow
                        |> updateAtFn
                            index
                            (fun (node, commit) ->
                                let updatedNode =
                                    node
                                    |> mapIf
                                        (index = endIndex)
                                        (fun node ->
                                            { node with
                                                Path.HalfTop = Some(GraphNodeReference.TargetColumnIndex endIndex)
                                                Path.HalfLeft = Some(GraphNodeReference.TargetColumnIndex endIndex)
                                            }
                                        )
                                    |> mapIf
                                        (index = startIndex)
                                        (fun node ->
                                            { node with
                                                Path.HalfRight = Some(GraphNodeReference.TargetColumnIndex endIndex)
                                            }
                                        )
                                    |> mapIf
                                        (index > endIndex && index < startIndex)
                                        (fun node ->
                                            { node with
                                                Path.HalfLeft = Some(GraphNodeReference.TargetColumnIndex endIndex)
                                                Path.HalfRight = Some(GraphNodeReference.TargetColumnIndex endIndex)
                                            }
                                        )

                                if index = endIndex then
                                    { updatedNode with Path.HalfBottom = None }, None // When we split a node, next row in graph won't have to follow this commit anymore
                                else
                                    updatedNode, commit
                            )
                    )
                )

            let (currentRow, mergeGraphIndexes) =
                ((currentRow, List.empty), (currentCommit.Parents |> Seq.indexed))
                ||> Seq.fold (fun (currentRow, indexes) (idx, parentCommit) ->
                    // idx = 0 for current node
                    // idx <> 0 for merge nodes where we try to find matching column index (junction point)
                    if idx = 0 then
                        currentRow, currentNodeIndex :: indexes
                    else
                        match tryFindChildNodeColumnIndexOfCommit parentCommit with
                        | Some idx ->
                            currentRow
                            |> updateAtFn
                                idx
                                (fun (node, _) ->
                                    { node with
                                        Path.HalfTop = Some(GraphNodeReference.TargetColumnIndex idx)
                                        Path.HalfBottom = None
                                    },
                                    Some parentCommit
                                ),
                            idx :: indexes
                        | None ->

                            let currentRow, insertIdx =
                                currentRow
                                |> insertAtFreeIndex (fun insertIdx ->
                                    ({ emptyNode with
                                        Path.HalfTop = Some(GraphNodeReference.TargetColumnIndex insertIdx)
                                        Path.HalfBottom = None
                                     },
                                     Some parentCommit)
                                )

                            currentRow, insertIdx :: indexes
                )

            let currentRow =
                (currentRow, mergeGraphIndexes |> List.pairwise)
                ||> List.fold (fun currentRow (endIndex, startIndex) ->
                    (currentRow, [ startIndex..endIndex ])
                    ||> List.fold (fun currentRow index ->
                        currentRow
                        |> updateAtFn
                            index
                            (fun (node, commit) ->
                                let updatedNode =
                                    node
                                    |> mapIf
                                        (index = endIndex)
                                        (fun node ->
                                            { node with
                                                Path.HalfBottom =
                                                    Some(GraphNodeReference.TargetColumnIndex endIndex)
                                                Path.HalfLeft = Some(GraphNodeReference.TargetColumnIndex endIndex)
                                            }
                                        )
                                    |> mapIf
                                        (index = startIndex)
                                        (fun node ->
                                            { node with
                                                Path.HalfRight = Some(GraphNodeReference.TargetColumnIndex endIndex)
                                            }
                                        )
                                    |> mapIf
                                        (index > endIndex && index < startIndex)
                                        (fun node ->
                                            { node with
                                                Path.HalfLeft = Some(GraphNodeReference.TargetColumnIndex endIndex)
                                                Path.HalfRight = Some(GraphNodeReference.TargetColumnIndex endIndex)
                                            }
                                        )

                                updatedNode, commit
                            )
                    )
                )

            let nextCommits =
                currentRow
                |> List.choose (fun (_, commit) -> commit)
                |> List.append nextCommits
                |> List.distinctBy (fun commit -> commit.Id)
                |> List.sortByDescending (fun commit -> commit.Committer.When)

            let trimmedRow =
                currentRow
                |> List.rev
                |> List.skipWhile (fun (node, followCommit) -> followCommit.IsNone && node = emptyNode)
                |> List.rev

            nextCommits, trimmedRow, Some currentCommit

    let loadGraphNodes refs =
        let getTargetCommit (ref: Reference) =
            ref.ResolveToDirectReference().Target |> unbox<Commit>

        let mutable commits = refs |> Seq.map getTargetCommit |> Seq.toList

        let mutable row = None

        seq {
            while row |> Option.isNone || row.Value <> List.empty do
                let nextCommits, nextRow, commit = computeNextGraphRow commits row
                row <- Some nextRow
                commits <- nextCommits

                match commit with
                | None -> ()
                | Some commit -> yield commit, nextRow |> List.map fst
        }