


// #r "nuget: FsToolkit.ErrorHandling"

// open FsToolkit.ErrorHandling

// let loadSettings () =
//     Result.Ok 12

// let load2 () =
//     if false then Result.Ok 10
//     else
//         Result.Error "parce que"

// let load3 () =
//     if false then Result.Ok ()
//     else Result.Error  "parce que"

// let test () :Result<_, string> =
//     result {
//         let! value = loadSettings ()
//         let! value2 = load2 ()
//         do! load3 ()

//         return! Result.Error "fin"  //value + 10 + value2
//     }

// test ()


#r "nuget: LibGit2Sharp"

#load "./src/FuGit/Domain/Types.fs"
#load "./src/FuGit/Features/Graph/Graph.Types.fs"

open FuGit
open LibGit2Sharp

let r = new Repository("/d/git/fugit/testdata/merge1")

let rows = Graph.RepositoryGraph.graphNodeSequence r.Refs |> Seq.toArray

// r.Commits.SortedBy <-  CommitSortStrategies.Time
r.Commits |> Seq.truncate 20 |> Seq.map (fun c -> c.MessageShort)

    // r.Branches[0].Commits
    // r.
r.Commits.QueryBy(
    CommitFilter(
        IncludeReachableFrom = r.Refs
        , SortBy = CommitSortStrategies.Time

    ))
    |> Seq.truncate 20
    |> Seq.map (fun c -> c.MessageShort)

// r.Network.Remotes
// c.Tree.Count

// r.Branches["main"].Commits
//     |> Seq.map (fun c -> c.MessageShort) |> Seq.truncate 20

let stashRefs =
    r.Refs
    |> Seq.filter (fun r ->
        r.CanonicalName.StartsWith("refs/stash")
    )
    |> Seq.toArray

let mainRefs =
    r.Refs
    |> Seq.filter (fun r ->
        r.CanonicalName.StartsWith("refs/heads/") |> not
        && r.CanonicalName.StartsWith("refs/tags/") |> not
    )
    |> Seq.toArray
    |> Seq.filter (fun r ->
        r.CanonicalName = "refs/heads/main"
        || r.CanonicalName = "refs/remotes/origin/main"
    ) |> Seq.toArray

// group each ref by name, for each group refs get list of commits

type BranchName =  BranchName of string list
type TagName =  TagName of string list
type RemoteName = RemoteName of string

[<RequireQualifiedAccess>]
type GitReference =
    | Head of BranchName
    | Remote of RemoteName * BranchName
    | Tag of TagName
    | Stash
    | Other of string list

module GitReference =
    let parse (ref:Reference) =
        match ref.CanonicalName.Split '/' |> Array.toList  with
        | "refs" :: "heads" :: name -> GitReference.Head (BranchName name)
        | "refs" :: "remotes" :: remoteName :: name -> GitReference.Remote (RemoteName remoteName, BranchName name)
        | "refs" :: "tags" :: tag -> GitReference.Tag (TagName tag)
        | "refs" :: [ "stash" ] -> GitReference.Stash
        | other -> GitReference.Other other

// let (|GitReference|_|) (ref:Reference) =
//     let refName = ref.CanonicalName.Split '/'
//     match refName with
//     | [| "refs"; "heads"; name |] -> GitReference.Head name |> Some
//     | [| "refs"; "remotes"; remoteName; name |] -> GitReference.Remote (remoteName, name) |> Some
//     | [| "refs"; "tags"; tag |] -> GitReference.Tag tag |> Some
//     | _ -> None

    // if ref.CanonicalName.StartsWith("refs/heads/") then
    //     Some ref.CanonicalName.[11..]
    // else None
    // match ref.CanonicalName with
    // | "refs/heads/" + name -> Some name
    // | _ -> None

 [ 5 down 2 ] |> List.pairwise

let rg =
    r.Refs
    |> Seq.filter ( fun r ->
        not r.IsTag
        && not r.IsRemoteTrackingBranch
    )
    |> Seq.toArray
    // |> Seq.map GitReference.parse
    // |> Seq.toArray
    // |> Seq.groupBy (fun gr -> gr)
    // |> Map.ofSeq

[ 1; 2; 3 ] |> List.rev |> List.pairwise

// let stashCommits =
//     rg
//     |> Map.tryFind GitReference.Stash
//     |> Option.bind Array.tryHead
//     |> Option.map (fun ref ->

//         r.Commits.QueryBy(
//             CommitFilter(
//                 IncludeReachableFrom = [| ref |]
//                 , SortBy = CommitSortStrategies.Time
//             ))
//             |> Seq.toArray
//     )

let refsByCommit =
    r.Refs
    |> Seq.map (fun ref ->
        let gitRef = GitReference.parse ref
        ref, gitRef
    )
    |> Seq.groupBy (fun (ref, _) -> ref.TargetIdentifier)
    |> Seq.map (fun (commitId, refs) -> commitId, refs |> Seq.toArray)
    |> Map.ofSeq


type CommitNode =
    {
        GraphColumnIndex: int
        Commit: Commit
        Refs: GitReference list
    }


let refsAndCommits =
    let refAndCommit (ref:Reference) =
        ref, (ref.ResolveToDirectReference().Target |> unbox<Commit>)
    [
        if not (r.Head.CanonicalName.StartsWith("refs")) then
            r.Head.Reference |> refAndCommit


        yield! r.Refs
                |> Seq.filter (fun ref -> not ref.IsTag)
                |> Seq.map refAndCommit
    ]
    |> List.sortByDescending (fun (r, commit) -> commit.Committer.When)

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

and ComposedNodeType = ComposedNodeType of NodeType list

[<RequireQualifiedAccess>]
type NodeDrawingType =
    | ``┌``
    | ``└``
    | ``┐``
    | ``┘``
    | ``│``
    | ``─``

let childNodes:(Node list) list = List.Empty

let (|PathWithBottom|_|) nodeType =
    match nodeType with
    | Path (path, columnIndex) ->
        path
        |> List.tryPick ( function
            | HorizontalToBottom node -> Some (node, columnIndex)
            | Vertical node -> Some (node, columnIndex)
            | _ -> None
        )
    | _ -> None


let updateOrInsert columnIndex commitRefColumnIndex rowNodes =
    match rowNodes |> List.tryItem columnIndex with
    | Some (NodeType.Empty columnIndex) ->
        fun newPath -> List.updateAt columnIndex (NodeType.Path ([ newPath ], commitRefColumnIndex) ) rowNodes
    | Some (NodeType.Path (path, columnIndex)) ->
        fun newPath -> List.updateAt columnIndex (NodeType.Path (newPath :: path, commitRefColumnIndex) ) rowNodes
    | Some (NodeType.Commit (_node, _columnIndex)) -> fun _newPath -> rowNodes
    | None -> fun newPath -> List.insertAt columnIndex (NodeType.Path ([ newPath ], commitRefColumnIndex) ) rowNodes

//let  (ref, currentCommit) :: nextRefsAndCommits = nRefsAndCommits
// let childNodes = n

let rec walkCommitAndRefsAndCreateParentsTree (refsAndCommits:(Reference * Commit) list) (childNodes:(NodeType list) list) =
    match refsAndCommits with
    | [] -> [], childNodes
    | (ref, currentCommit) :: nextRefsAndCommits ->
        // When computing a new commit line, we should transform previous node type to new node type
        let currentRow =
            childNodes
            |> List.tryHead
            |> Option.defaultValue List.empty
            |> List.map (fun nodeType ->
                match nodeType with
                | NodeType.Commit (node, columnIndex) -> Path ([ Vertical node ], columnIndex)
                | NodeType.Empty columnIndex -> Empty columnIndex
                | PathWithBottom (node, columnIndex)  -> Path ([ Vertical node ], columnIndex)
                | NodeType.Path (path, columnIndex) -> Empty columnIndex
            )

        let tryFindColumnIndexOfCommit c =
            currentRow
            |> List.tryFindIndex (fun nodeType ->
                match nodeType with
                | NodeType.Commit (node, _)
                | PathWithBottom (node, _) ->  node.Parent = Some c
                | NodeType.Empty _
                | NodeType.Path _ -> false)



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
                        { Reference = ref; Commit = currentCommit; Parent = Some parentCommit }
                    node, columnIndex
                )
                |> Seq.toList

            if nodes.Length = 0 then
                let node =
                    { Reference = ref; Commit = currentCommit; Parent = None }
                [ node, None ]
            else nodes
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
                    commitColumnIndexMemo
                    |> Option.defaultValue currentColumnIndex


                let newRowNodes =
                    if commitColumnIndexMemo.IsNone then
                        // Commit node is created on first iteration
                        // Other nodes are Merge nodes
                        let updateOrInsert =
                            if currentColumnIndex < rowNodes.Length then
                                List.updateAt
                            else
                                List.insertAt

                        rowNodes |> updateOrInsert currentColumnIndex (NodeType.Commit (node, currentColumnIndex))
                    else
                        let pathRange =
                            if commitColumnIndex < currentColumnIndex then [ commitColumnIndex + 1 .. currentColumnIndex - 1 ]
                            else [ currentColumnIndex + 1 .. commitColumnIndex - 1 ]

                        // Fill horizontal path
                        let rowNodes =
                            (rowNodes, pathRange)
                            ||> List.fold (fun rowNodes columnIndex ->
                                updateOrInsert columnIndex commitColumnIndexMemo.Value rowNodes PathNode.Horizontal
                            )

                        // set merge node
                        updateOrInsert currentColumnIndex commitColumnIndexMemo.Value rowNodes (PathNode.HorizontalToBottom node)

                        // NodeType.Merge (node, currentColumnIndex, commitColumnIndex)

                commitColumnIndexMemo  <- Some commitColumnIndex

                newRowNodes
            )

                // currentCommit.Parents
                // |> Seq.map (fun parentCommit ->
                //         ref, parentCommit
                //     )
                // |> Seq.toList
                // |> List.sortByDescending (fun (_ref, commit) -> commit.Committer.When)
        let newRefsAndCommits =
            [
                match currentCommit.Parents |> Seq.tryHead with
                | None -> ()
                | Some parentCommit -> (ref, parentCommit)

                yield! nextRefsAndCommits
            ]
            |> List.sortByDescending (fun (_ref, commit) -> commit.Committer.When)


        (newRefsAndCommits, rowNodes :: childNodes)

let (nRefsAndCommits, n)  = walkCommitAndRefsAndCreateParentsTree refsAndCommits [ ]
let (nRefsAndCommits2,n2)  = walkCommitAndRefsAndCreateParentsTree nRefsAndCommits n
let (nRefsAndCommits3,n3)  = walkCommitAndRefsAndCreateParentsTree nRefsAndCommits2 n2
let (nRefsAndCommits4,n4)  = walkCommitAndRefsAndCreateParentsTree nRefsAndCommits3 n3
let (nRefsAndCommits5,n5)  = walkCommitAndRefsAndCreateParentsTree nRefsAndCommits4 n4



let l = [ 1; 2; 3]

match l with
| [] -> None
| [ x ] -> Some x
| [ 1; x; 3 ] -> Some x
| head :: tail -> Some x

// let rec walkCommitAndRefsAndCreateParentsTree (refsAndCommits:(Reference * Commit) list) (childNodes:(NodeType list) list) =
//     match refsAndCommits |> Seq.tryHead with
//     | None -> []
//     | Some (ref, currentCommit) ->
//         // When computing a new commit line, we should transform previous node type to new node type
//         let currentRow =
//             childNodes
//             |> List.tryHead
//             |> Option.defaultValue List.empty
//             |> List.map (fun nodeType ->
//                 match nodeType with
//                 | NodeType.Commit (node, columnIndex) -> Path [ Vertical (node, columnIndex) ]
//                 | NodeType.HorizontalNoOp (_node, columnIndex) -> Empty columnIndex
//                 | NodeType.Split (_node, columnIndex, _refCommitColumnIndex) -> Empty columnIndex
//                 | NodeType.VerticalNoOp (node, columnIndex) -> VerticalNoOp (node, columnIndex)
//                 | NodeType.Merge (node, columnIndex, _refCommitColumnIndex) -> VerticalNoOp (node, columnIndex)
//                 | NodeType.Empty columnIndex -> Empty columnIndex
//             )

//         let tryFindColumnIndexOfCommit c =
//             currentRow
//             |> List.tryFindIndex (fun nodeType ->
//                 match nodeType with
//                 | NodeType.Commit (node, _)
//                 | NodeType.VerticalNoOp (node, _)
//                 | NodeType.Merge (node, _, _)
//                 | NodeType.Split (node, _, _) ->  node.Parent = Some c
//                 | NodeType.Empty _
//                 | NodeType.HorizontalNoOp _ -> false)



//         let commitNodes =
//             let nodes =
//                 // current commit not found in previous row, it's a new brand start point
//                 currentCommit.Parents
//                 |> Seq.map (fun parentCommit ->
//                     let node =
//                         { Reference = ref; Commit = currentCommit; Parent = Some parentCommit }
//                     node, tryFindColumnIndexOfCommit parentCommit
//                 )
//                 |> Seq.toList

//             if nodes.Length = 0 then
//                 let node =
//                     { Reference = ref; Commit = currentCommit; Parent = None }
//                 [ node, None ]
//             else nodes
//         printfn "commitNodes %A" commitNodes


//         let rowNodes =
//             let mutable commitColumnIndexMemo = None
//             ((currentRow, List.empty), commitNodes)
//             ||> List.fold (fun (rowNodes, nodesWithIndex)  (node, nodeIndex) ->

//                 let currentColumnIndex =
//                     nodeIndex // matching column from previous commit
//                     |> Option.defaultValue rowNodes.Length // column index from where we will insert the new node

//                 let commitColumnIndex =
//                     commitColumnIndexMemo
//                     |> Option.defaultValue currentColumnIndex


//                 let nodeType =
//                     if commitColumnIndexMemo.IsNone then
//                         // Commit node is created on first iteration
//                         // Other nodes are Merge nodes
//                         NodeType.Commit (node, currentColumnIndex)
//                     else
//                         NodeType.Merge (node, currentColumnIndex, commitColumnIndex)

//                 commitColumnIndexMemo  <- Some commitColumnIndex

//                 match nodeIndex with
//                 | None ->
//                     rowNodes |> List.insertAt rowNodes.Length nodeType
//                     , (node, rowNodes.Length) :: nodesWithIndex
//                 | Some nodeIndex ->
//                     rowNodes |> List.updateAt nodeIndex nodeType
//                     , (node, nodeIndex) :: nodesWithIndex
//             )
//             ||> List.fold (fun rowNodes nodesWithIndex ->

//                     rowNodes
//             )

//         // let r =
//         //     match newNodesWithIndex |> List.rev with
//         //     | [] -> []
//         //     | (commitNode, commitNodeIndex) :: mergeNodes ->
//         //         NodeType.Commit (commitNode, commitNodeIndex)
//         //         :: (mergeNodes
//         //                 |> List.map (fun (node, nodeIndex) -> NodeType.Merge (node, commitNodeIndex, nodeIndex))
//         //             )



//         // let baseCommit = newNodesWithIndex

//         // newNodesWithIndex
//         rowNodes
// let l = [ 'a'; 'b'; 'c' ]
// let l2 = 'x' :: l

    // // |> Seq.toList
    //         // |> List.rev
    //         // @ branchesNodes
    //         (+) currentRow (walkCommitAndRefsAndCreateParentsTree (refsAndCommits |> Seq.skip 1) (currentRow :: childNodes))
    //     | Some nodeIndex ->
    //         // branchesNodes
            // |> List.updateAt nodeIndex (fun (node, ref, commit) ->
            //     Node.Commit (ref, currentCommit)
            // )

                // Node.Commit refAndCommit :: branchesNodes.[0..c-1] @ [ Node.NoOp ref ] @ branchesNodes.[c+1..]
            // if branchesNodes.Length = 0 then
            //     [  refAndCommit ]
        // ()
        // |> Seq.sortBy

    // let currentBranches =
    // ()
    // seq {
    //     while
    // }



let ch = r.Head.Reference.ResolveToDirectReference().Target |> unbox<Commit>
ch.Parents |> Seq.head = ((refAndCommit[2] |> snd).Parents |> Seq.head)
// r.Head.Reference

// r.Head.Commits |> Seq.head
let allCommits =
    let commits =
        r.Commits.QueryBy(
            CommitFilter(
                IncludeReachableFrom =
                    [| "HEAD"; yield! r.Refs |> Seq.map (fun ref -> ref.CanonicalName) |]

                , SortBy = CommitSortStrategies.Time
            ))
        |> Seq.toArray

    let nodes =
        let mutable previousNodes = List.Empty
        Array.init
            commits.Length
            (fun idx ->

                let newBranch =
                    refsByCommit
                    |> Map.tryFind commits[idx].Id

                let freeColumnIndex =
                    previousNodes
                    |> List.tryFind (fun node -> )

                let node =
                    {   GraphColumnIndex = 0
                        Commit = commits[idx]
                    }
                previousNodes <- [ node, node.Commit.Parents |> Seq.toArray ]

                // | Some node
                // { GraphColumnIndex = -1; Children = [] }
            )
        // commits
        // |> Array.mapi (fun (nodes, index) commit ->
        //     let node = { GraphColumnIndex = index; Children = [] }
        //     let nodes = Map.add commit.Id node nodes
        //     let index = index + 1
        //     nodes, index
        // )

r.Commits

let c = r.Lookup<Commit>("1cfa716aba1ed8ce1d0430e0d92990a3088fb6e9")
c
let c = r.Lookup<Commit>("aa1d0c1244442b070348cbe56c715aa60b537fe6")
let cp1 = c.Parents |> Seq.head
let cp2 = c.Parents |> Seq.skip 1 |> Seq.head


let cmts =
    commits
    |> Array.skipWhile (fun (c:Commit) -> c.Id.Sha <> "aa1d0c1244442b070348cbe56c715aa60b537fe6")

cmts[0]
cmts[1]

let test = stashCommits |> Option.get
test[0]

rg
|> Array.filter (fun (gr, _) ->
    match gr with
    | GitReference.Other _ -> true
    | _ -> false
)

let refsByBranchName =
    r.Refs
    |> Seq.choose (fun (r:Reference) ->
        match r |> GitReference.parse with
        | Some ((GitReference.Head name) as gr) -> Some (name, gr)
        | Some ((GitReference.Remote (_, name) as gr)) -> Some (name, gr)
        | _ -> None
    )
    |> Seq.groupBy (fun (name, _) -> name)
    |> Seq.map (fun (name, refs) -> name, refs |> Seq.map snd |> Seq.toArray)
    |> Seq.toArray


let commitsByBranch =
    refsByBranchName
    |> Array.map (fun (name, refs) ->
        let commits =
            r.Commits.QueryBy(
                CommitFilter(
                    IncludeReachableFrom = r.Refs,
                    SortBy = CommitSortStrategies.Time
                )
            )
            |> Seq.toArray
        name, commits
    )

// let mainCommits =
//     r.Commits.QueryBy(
//         CommitFilter(
//             IncludeReachableFrom = mainRefs
//             , SortBy = CommitSortStrategies.Time
//         ))
//         // |> Seq.map (fun c -> c.MessageShort)
//         |> Seq.truncate 20
//         |> Seq.toArray
//         // |> Seq.head
// // .Contains "main" && not r.IsTag ) |> Seq.toArray

// // r.Branches["main"].