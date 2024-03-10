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

type GraphShape =
    | Start // ╷
    | End // ╵
    | Vertical // │
    | Horizontal of GraphNodeReference // ─
    | HorizontalRightToBottom of GraphNodeReference // ┐
    | HorizontalLeftToBottom of GraphNodeReference // ┌
    | HorizontalRight of GraphNodeReference // ╶
    | HorizontalLeft of GraphNodeReference // ╴
    | VerticalTopToRight of GraphNodeReference // └
    | VerticalTopToLeft of GraphNodeReference // ┘

// ╴
// ╶
// ╵
// ╷
// ┐
// ┌
// ┘
// └
// │
// ─


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
        TargetParentCommit: Commit option
        // Path: GraphNodePath
        Shapes: GraphShape list
    }

type GraphRow = { Cells: GraphNode option list }

// module GraphNode =
//     let empty =
//         {
//             Commit = None
//             TargetParentCommit = None
//             Shapes = []
//         }
// [<RequireQualifiedAccess>]
// type NodeDrawingType =
//     | ``┌``
//     | ``└``
//     | ``┐``
//     | ``┘``
//     | ``│``
//     | ``─``

module RepositoryGraph =

    let loadRefsAndCommits (repository: Repository) =
        let refAndCommit (ref: Reference) =
            ref, (ref.ResolveToDirectReference().Target |> unbox<Commit>)

        [
            if not (repository.Head.CanonicalName.StartsWith("refs")) then
                repository.Head.Reference |> refAndCommit


            yield! repository.Refs |> Seq.filter (fun ref -> not ref.IsTag) |> Seq.map refAndCommit
        ]
        |> List.sortByDescending (fun (_r, commit) -> commit.Committer.When)


    let mapIf condition map value =
        if condition then
            map value
        else
            value


    let pairHeadWithTailItems (lst: 'a list) =
        if lst.Length > 1 then
            let head = lst |> Seq.head
            lst |> List.skip 1 |> List.rev |> List.map (fun index -> head, index)
        else
            []

    // [<RequireQualifiedAccess>]
    // type GraphCell =
    //     | Node of GraphNode
    //     | Empty

    let updateAtFn index map list =
        let value = list |> List.item index
        list |> List.updateAt index (map value)

    module GraphRow =
        let empty = { Cells = List.empty }

        let trim (row: GraphRow) =
            {
                Cells =
                    row.Cells
                    |> List.rev
                    |> List.skipWhile (fun cell -> cell.IsNone || cell.Value.Shapes = List.empty)
                    |> List.rev
            }

        // let tryFindChildNodeColumnIndexOfCommit commit (row:GraphRow) =
        //     row.Cells
        //     |> List.tryFindIndex (function
        //         | Some cell -> cell.TargetParentCommit = Some commit
        //         | None -> false)

        let childNodeIndexesOfMatchingCommit commit (row: GraphRow) =
            row.Cells
            |> List.indexed
            |> List.choose (
                function
                | (idx, Some cell) when cell.TargetParentCommit = Some commit -> Some idx
                | _ -> None
            )

        let createNewFollowRow (row: GraphRow) =
            {
                Cells =
                    row.Cells
                    |> List.map (
                        function
                        | Some({ TargetParentCommit = Some _ } as previousCell) ->
                            Some
                                { previousCell with
                                    Shapes = [ Vertical ]
                                    Commit = None
                                }
                        | _ -> None
                    )
            }

        let insertAtFreeIndex map (row: GraphRow) =
            let cells, idx =
                match row.Cells |> List.tryFindIndex (fun cell -> cell.IsNone) with
                | Some idx -> row.Cells |> List.updateAt idx (map idx |> Some), idx
                | None -> row.Cells |> List.insertAt row.Cells.Length (map row.Cells.Length |> Some), row.Cells.Length

            { Cells = cells }, idx

        let updateAtFn index map (row: GraphRow) =
            { Cells = row.Cells |> updateAtFn index map }

        let commits (row: GraphRow) =
            row.Cells
            |> List.choose (
                function
                | Some cell -> cell.Commit
                | None -> None
            )

        let targetCommits (row: GraphRow) =
            row.Cells
            |> List.choose (
                function
                | Some cell -> cell.TargetParentCommit
                | None -> None
            )

    let computeNextGraphRow (commits: Commit list) (previousNodesRow: GraphRow option) =
        match
            commits
            |> List.distinctBy (fun commit -> commit.Id)
            |> List.sortByDescending (fun commit -> commit.Committer.When)
        with
        | [] -> [], GraphRow.empty, None
        | (currentRowCommit) :: nextCommits ->

            let currentRow =
                previousNodesRow
                |> Option.defaultValue GraphRow.empty
                |> GraphRow.trim
                |> GraphRow.createNewFollowRow

            let childNodeIndexesOfCurrentRowCommit =
                currentRow |> GraphRow.childNodeIndexesOfMatchingCommit currentRowCommit

            let tryFindChildNodeColumnIndexOfCommit commit =
                currentRow |> GraphRow.childNodeIndexesOfMatchingCommit commit |> Seq.tryHead


            // Set current commit node
            let currentRow, currentNodeIndex =
                let firstParentOfCurrentCommit = currentRowCommit.Parents |> Seq.tryHead

                match childNodeIndexesOfCurrentRowCommit |> Seq.tryHead with
                | None ->
                    // Not a parent of previous row, insert a new Cell
                    currentRow
                    |> GraphRow.insertAtFreeIndex (fun _ ->
                        match firstParentOfCurrentCommit with
                        | Some parent ->
                            // Current Commit have parents, Shape will be a start
                            {
                                Commit = Some currentRowCommit
                                TargetParentCommit = Some parent
                                Shapes = [ GraphShape.Start ]
                            }
                        | None ->
                            // No parent, No vertical shape
                            {
                                Commit = Some currentRowCommit
                                TargetParentCommit = None
                                Shapes = []
                            }
                    )
                | Some index ->
                    // Current commit is a parent of previous row, set it at the first index
                    (currentRow
                     |> GraphRow.updateAtFn
                         index
                         (fun _cell ->
                             match firstParentOfCurrentCommit with
                             | Some parent ->
                                 Some
                                     {
                                         Commit = Some currentRowCommit
                                         TargetParentCommit = Some parent
                                         Shapes = [ GraphShape.Vertical ]
                                     }
                             | None ->
                                 // No parent, Current commit is the end of branch
                                 Some
                                     {
                                         Commit = Some currentRowCommit
                                         TargetParentCommit = None
                                         Shapes = [ GraphShape.End ]
                                     }
                         )),
                    index


            let splitGraphIndexes = childNodeIndexesOfCurrentRowCommit |> pairHeadWithTailItems

            // Split nodes implies that child nodes are already existing in the previous row
            // Create horizontal lines and angles to connect cells

            let updateNode shape mapTargetParent =
                function
                | Some node ->
                    // let targetParentCommit =
                    //     match shape with
                    //     | GraphShape.VerticalTopToLeft _ -> None
                    //     | _ -> node.TargetParentCommit

                    Some
                        { node with
                            Shapes = shape :: node.Shapes
                            TargetParentCommit = node.TargetParentCommit |> mapTargetParent
                        }

                | None ->
                    Some
                        {
                            Commit = None
                            TargetParentCommit = None |> mapTargetParent //Some currentRowCommit
                            // Path: GraphNodePath
                            Shapes = [ shape ]
                        }


            let currentRow =
                (currentRow, splitGraphIndexes)
                ||> List.fold (fun currentRow (startIndex, endIndex) ->
                    (currentRow, [ startIndex..endIndex ])
                    ||> List.fold (fun currentRow index ->
                        currentRow
                        |> GraphRow.updateAtFn
                            index
                            (fun node ->
                                node
                                |> mapIf
                                    (index = endIndex)
                                    (if startIndex < endIndex then
                                         updateNode
                                             (GraphShape.VerticalTopToLeft(
                                                 GraphNodeReference.TargetColumnIndex endIndex
                                             ))
                                             (fun _ -> None)
                                     else
                                         updateNode
                                             (GraphShape.VerticalTopToRight(
                                                 GraphNodeReference.TargetColumnIndex endIndex
                                             ))
                                             (fun _ -> None))
                                |> mapIf
                                    (index = startIndex)
                                    (if startIndex < endIndex then
                                         updateNode
                                             (GraphShape.HorizontalRight(
                                                 GraphNodeReference.TargetColumnIndex endIndex
                                             ))
                                             id
                                     else
                                         updateNode
                                             (GraphShape.HorizontalLeft(
                                                 GraphNodeReference.TargetColumnIndex endIndex
                                             ))
                                             id)
                                |> mapIf
                                    (index > startIndex && index < endIndex)
                                    (updateNode
                                        (GraphShape.Horizontal(GraphNodeReference.TargetColumnIndex endIndex))
                                        id)
                            )
                    )
                )

            let (currentRow, mergeGraphIndexes) =
                // Take each parents of current commit and try to match an existing cell target or insert a new cell
                ((currentRow, List.empty), (currentRowCommit.Parents |> Seq.indexed))
                ||> Seq.fold (fun (currentRow, indexes) (idx, currentRowParentCommit) ->
                    // idx = 0 for current node
                    // idx <> 0 for merge nodes where we try to find matching column index (junction point)
                    if idx = 0 then
                        currentRow, (currentNodeIndex, currentRowParentCommit) :: indexes
                    else
                        match tryFindChildNodeColumnIndexOfCommit currentRowParentCommit with
                        | Some idx ->
                            currentRow
                            |> GraphRow.updateAtFn
                                idx
                                (function
                                | Some node ->
                                    Some
                                        { node with
                                            Shapes =
                                                if idx > currentNodeIndex then
                                                    GraphShape.HorizontalRightToBottom(
                                                        GraphNodeReference.TargetColumnIndex idx
                                                    )
                                                else
                                                    GraphShape.HorizontalLeftToBottom(
                                                        GraphNodeReference.TargetColumnIndex idx
                                                    )
                                                :: node.Shapes
                                        }
                                | None ->
                                    // Should not happen as we found a matching column index before
                                    Some
                                        {
                                            Commit = None
                                            TargetParentCommit = Some currentRowParentCommit
                                            Shapes =
                                                [
                                                    if idx > currentNodeIndex then
                                                        GraphShape.HorizontalRightToBottom(
                                                            GraphNodeReference.TargetColumnIndex idx
                                                        )
                                                    else
                                                        GraphShape.HorizontalLeftToBottom(
                                                            GraphNodeReference.TargetColumnIndex idx
                                                        )
                                                ]
                                        }
                                ),
                            (idx, currentRowParentCommit) :: indexes
                        | None ->

                            let currentRow, insertIdx =
                                currentRow
                                |> GraphRow.insertAtFreeIndex (fun insertIdx ->
                                    ({
                                        Commit = None
                                        TargetParentCommit = Some currentRowParentCommit
                                        Shapes =
                                            [
                                                if insertIdx > currentNodeIndex then
                                                    GraphShape.HorizontalRightToBottom(
                                                        GraphNodeReference.TargetColumnIndex insertIdx
                                                    )
                                                else
                                                    GraphShape.HorizontalLeftToBottom(
                                                        GraphNodeReference.TargetColumnIndex insertIdx
                                                    )
                                            ]
                                    })
                                )

                            currentRow, (insertIdx, currentRowParentCommit) :: indexes
                )


            let currentRow =
                (currentRow, mergeGraphIndexes |> pairHeadWithTailItems)
                ||> List.fold (fun currentRow ((endIndex, endParentCommit), (startIndex, starParentCommit)) ->

                    (currentRow,
                     if startIndex < endIndex then
                         [ startIndex..endIndex ]
                     else
                         [ endIndex..startIndex ])
                    ||> List.fold (fun currentRow index ->
                        currentRow
                        |> GraphRow.updateAtFn
                            index
                            (fun node ->
                                node
                                |> mapIf
                                    (index = endIndex)
                                    (if startIndex < endIndex then
                                         updateNode
                                             (GraphShape.HorizontalLeftToBottom(
                                                 GraphNodeReference.TargetColumnIndex endIndex
                                             ))
                                             (fun _ -> Some endParentCommit)
                                     else
                                         updateNode
                                             (GraphShape.HorizontalRightToBottom(
                                                 GraphNodeReference.TargetColumnIndex endIndex
                                             ))
                                             (fun _ -> Some endParentCommit))
                                |> mapIf
                                    (index = startIndex)
                                    (if startIndex < endIndex then
                                         updateNode
                                             (GraphShape.HorizontalRight(
                                                 GraphNodeReference.TargetColumnIndex endIndex
                                             ))
                                             (fun _ -> Some starParentCommit)
                                     else
                                         updateNode
                                             (GraphShape.HorizontalLeft(
                                                 GraphNodeReference.TargetColumnIndex endIndex
                                             ))
                                             (fun _ -> Some starParentCommit))
                                |> mapIf
                                    (index > startIndex && index < endIndex)
                                    (updateNode
                                        (GraphShape.Horizontal(GraphNodeReference.TargetColumnIndex endIndex))
                                        id)
                            )
                    )
                )

            let currentRow =
                // Clear vertical lines on node that have no follow commit
                {
                    Cells =
                        currentRow.Cells
                        |> List.map (
                            function
                            | Some cell when cell.TargetParentCommit.IsNone ->
                                Some
                                    { cell with
                                        Shapes =
                                            cell.Shapes
                                            |> List.filter (
                                                function
                                                | GraphShape.Vertical -> false
                                                | _ -> true
                                            )
                                    }
                            | cell -> cell
                        )
                }

            let nextCommits =
                currentRow
                |> GraphRow.targetCommits
                |> List.append nextCommits
                |> List.distinctBy (fun commit -> commit.Id)
                |> List.sortByDescending (fun commit -> commit.Committer.When)

            nextCommits, currentRow, Some currentRowCommit

    let loadGraphNodes refs =
        let getTargetCommit (ref: Reference) =
            ref.ResolveToDirectReference().Target |> unbox<Commit>

        let mutable commits = refs |> Seq.map getTargetCommit |> Seq.toList

        let mutable row: GraphRow option = None

        seq {
            while row |> Option.isNone || row.Value.Cells <> List.empty do
                let nextCommits, nextRow, commit = computeNextGraphRow commits row
                row <- Some nextRow
                commits <- nextCommits

                match commit with
                | None -> ()
                | Some commit -> yield commit, nextRow.Cells
        }