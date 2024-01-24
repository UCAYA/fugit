namespace FuGit.Graph

open LibGit2Sharp

// r.Head
type Node =
    {
        Reference: Reference
        Commit: Commit
        Parent: Commit option
    }

// type PathId = PathId of int

type PathNode =
    | Vertical of Node
    | HorizontalToBottom of Node
    | TopToHorizontal of Node
    | Horizontal

type NodeType =
    | Commit of Node * int
    | Empty of int
    | Path of PathNode list * int

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

    let childNodes: (Node list) list = List.Empty

    let (|PathWithBottom|_|) nodeType =
        match nodeType with
        | Path(path, columnIndex) ->
            path
            |> List.tryPick (
                function
                | HorizontalToBottom node -> Some(node, columnIndex)
                | Vertical node -> Some(node, columnIndex)
                | _ -> None
            )
        | _ -> None


    let updateOrInsert columnIndex commitRefColumnIndex rowNodes =
        match rowNodes |> List.tryItem columnIndex with
        | Some(NodeType.Empty columnIndex) ->
            fun newPath -> List.updateAt columnIndex (NodeType.Path([ newPath ], commitRefColumnIndex)) rowNodes
        | Some(NodeType.Path(path, columnIndex)) ->
            fun newPath -> List.updateAt columnIndex (NodeType.Path(newPath :: path, commitRefColumnIndex)) rowNodes
        | Some(NodeType.Commit(_node, _columnIndex)) -> fun _newPath -> rowNodes
        | None -> fun newPath -> List.insertAt columnIndex (NodeType.Path([ newPath ], commitRefColumnIndex)) rowNodes

    //let  (ref, currentCommit) :: nextRefsAndCommits = nRefsAndCommits
    // let childNodes = n

    let rec walkCommitAndRefsAndCreateParentsTree
        (refsAndCommits: (Reference * Commit) list)
        (previousNodesRow: (NodeType list) option)
        =
        let currentRow =
            previousNodesRow
            |> Option.defaultValue List.empty
            |> List.map (fun nodeType ->
                match nodeType with
                | NodeType.Commit(node, columnIndex) -> Path([ Vertical node ], columnIndex)
                | NodeType.Empty columnIndex -> Empty columnIndex
                | PathWithBottom(node, columnIndex) -> Path([ Vertical node ], columnIndex)
                | NodeType.Path(_path, columnIndex) -> Empty columnIndex
            )

        match
            refsAndCommits
            |> List.sortByDescending (fun (_ref, commit) -> commit.Committer.When)
        with
        | [] -> [], currentRow
        | (ref, currentCommit) :: nextRefsAndCommits ->
            // When computing a new commit line, we should transform previous node type to new node type
            let tryFindColumnIndexOfCommit c =
                currentRow
                |> List.tryFindIndex (fun nodeType ->
                    match nodeType with
                    | NodeType.Commit(node, _)
                    | PathWithBottom(node, _) -> node.Parent = Some c
                    | NodeType.Empty _
                    | NodeType.Path _ -> false
                )

            let commitNodes =
                let nodes =
                    // current commit not found in previous row, it's a new start point
                    currentCommit.Parents
                    |> Seq.mapi (fun idx parentCommit ->
                        // idx = 0 for current node
                        // idx <> 0 for merge nodes where we try to find matching column index (junction point)
                        let columnIndex =
                            if idx = 0 then
                                tryFindColumnIndexOfCommit currentCommit
                            else
                                tryFindColumnIndexOfCommit parentCommit

                        let node =
                            {
                                Reference = ref
                                Commit = currentCommit
                                Parent = Some parentCommit
                            }

                        node, columnIndex
                    )
                    |> Seq.toList

                if nodes.Length = 0 then
                    let node =
                        {
                            Reference = ref
                            Commit = currentCommit
                            Parent = None
                        }

                    [ node, None ]
                else
                    nodes
            // printfn "commitNodes %A" commitNodes


            let rowNodes =
                // let [ (node, nodeIndex) ] = commitNodes
                let mutable commitColumnIndexMemo = None

                (currentRow, commitNodes)
                ||> List.fold (fun rowNodes (node, nodeIndex) ->
                    let currentColumnIndex =
                        nodeIndex // matching column from previous commit
                        |> Option.defaultValue rowNodes.Length // column index from where we will insert the new node

                    let commitColumnIndex =
                        commitColumnIndexMemo |> Option.defaultValue currentColumnIndex


                    let newRowNodes =
                        if commitColumnIndexMemo.IsNone then
                            // Commit node is created on first iteration
                            // Other nodes are Merge nodes
                            let updateOrInsert =
                                if currentColumnIndex < rowNodes.Length then
                                    List.updateAt
                                else
                                    List.insertAt

                            rowNodes
                            |> updateOrInsert currentColumnIndex (NodeType.Commit(node, currentColumnIndex))
                        else
                            let pathRange =
                                if commitColumnIndex < currentColumnIndex then
                                    [ commitColumnIndex + 1 .. currentColumnIndex - 1 ]
                                else
                                    [ currentColumnIndex + 1 .. commitColumnIndex - 1 ]

                            // Fill horizontal path
                            let rowNodes =
                                (rowNodes, pathRange)
                                ||> List.fold (fun rowNodes columnIndex ->
                                    updateOrInsert
                                        columnIndex
                                        commitColumnIndexMemo.Value
                                        rowNodes
                                        PathNode.Horizontal
                                )

                            // set merge node
                            updateOrInsert
                                currentColumnIndex
                                commitColumnIndexMemo.Value
                                rowNodes
                                (PathNode.HorizontalToBottom node)

                    // NodeType.Merge (node, currentColumnIndex, commitColumnIndex)

                    commitColumnIndexMemo <- Some commitColumnIndex

                    newRowNodes
                )

            let newRefsAndCommits =
                [
                    match currentCommit.Parents |> Seq.tryHead with
                    | None -> ()
                    | Some parentCommit -> (ref, parentCommit)

                    yield! nextRefsAndCommits
                ]

            (newRefsAndCommits, rowNodes)