module FuGit.Features.Graph.View

open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Data
open Avalonia.Collections
open Avalonia.FuncUI.Hosts
open Avalonia.Themes.Fluent
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open System.Threading.Tasks
open FuGit.Features.Graph

// open System.Collections
// open System.Collections.Generic
// open System.Collections.ObjectModel

open LibGit2Sharp

let r = new Repository("/d/git/lavitre-app/")

let refs = r.Refs |> Seq.filter (fun t -> not t.IsTag)
let commits = RepositoryGraph.loadGraphNodes refs //|> Seq.toArray


let allcommits =
    RepositoryGraph.loadGraphNodes refs
    |> Seq.truncate 30
    |> Seq.map (fun x -> x |> (fun (a, b) -> a.MessageShort, b))
    |> Seq.toArray


printfn "allcommits %A" allcommits

[<AutoOpen>]
module DataGridExt =
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.Builder

    type DataGrid with

        static member gridLinesVisibility<'t when 't :> DataGrid>
            (gridLinesVisibility: DataGridGridLinesVisibility)
            : IAttr<'t>
            =
            AttrBuilder<'t>
                .CreateProperty<DataGridGridLinesVisibility>(
                    DataGrid.GridLinesVisibilityProperty,
                    gridLinesVisibility,
                    ValueNone
                )

        static member rowHeight<'t when 't :> DataGrid>(rowHeight: float) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<float>(DataGrid.RowHeightProperty, rowHeight, ValueNone)

    type DataGridColumn with

        static member cellStyleClasses<'t when 't :> DataGridColumn>(value: string list) : IAttr<'t> =
            let getter: ('t -> string list) =
                (fun control -> control.CellStyleClasses |> Seq.map id |> List.ofSeq)

            let setter: ('t * string list -> unit) =
                (fun (control, value) ->
                    control.CellStyleClasses.Clear()
                    control.CellStyleClasses.AddRange(value)
                )

            AttrBuilder<'t>
                .CreateProperty<string list>("CellStyleClasses", value, ValueSome getter, ValueSome setter, ValueNone)


let brushes =
    [
        "#49045c"
        "#720b5f"
        "#961f60"
        "#b5395f"
        "#cf555e"
        "#e4745e"
        "#f49462"
        "#f9bf63"
        "#e3d55b"
        "#d2e05e"
        "#beeb66"
        "#85ff87"
        "#00f6aa"
        "#00e9d1"
        "#00d9f6"
        "#00c7ff"
        "#00b1ff"
        "#0096ff"
        "#5775ff"
        "#6254cd"
    ]
    |> List.map Media.Brush.Parse

let getBrushFromIndex index =
    let i = index % (brushes.Length - 1)
    brushes[i]

[<AutoOpen>]
module ItemsRepeater =
    open Avalonia.Controls.Templates
    open System.Collections
    open Avalonia.Controls
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.Builder

    let create (attrs: IAttr<ItemsRepeater> list) : IView<ItemsRepeater> =
        ViewBuilder.Create<ItemsRepeater>(attrs)

    type ItemsRepeater with

        static member dataItems<'t when 't :> ItemsRepeater>(data: IEnumerable) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<IEnumerable>(ItemsRepeater.ItemsSourceProperty, data, ValueNone)

        static member itemTemplate<'t when 't :> ItemsRepeater>(value: IDataTemplate) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<IDataTemplate>(ItemsRepeater.ItemTemplateProperty, value, ValueNone)

        static member onItemsChanged<'t when 't :> ItemsRepeater>(func: IEnumerable -> unit, ?subPatchOptions) =
            AttrBuilder<'t>
                .CreateSubscription(
                    ItemsRepeater.ItemsSourceProperty :> AvaloniaProperty<IEnumerable>,
                    func,
                    ?subPatchOptions = subPatchOptions
                )

open Avalonia.Controls
open Avalonia.Styling
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.VirtualDom


type LazyDataSource<'a>(source: System.Collections.Generic.IEnumerable<'a>, pageSize: int) as self =
    inherit AvaloniaList<LazyLoadableItem<'a>>()

    let createItem loadableData = LazyLoadableItem(loadableData, self)
    let createLoadedItem data = data |> Loadable.Loaded |> createItem
    let createLoadingItem () = Loadable.Loading |> createItem

    let iterator = source.GetEnumerator()

    do
        self.Add(LazyLoadableItem(Loadable.Loading, self))
        self.LoadNextPage()

    member _.LoadNextPage() =
        let last = self |> Seq.tryLast

        match last with
        | Some last when last.IsLoading ->
            let page = ResizeArray<_>(pageSize)

            while page.Count < pageSize && iterator.MoveNext() do
                iterator.Current |> createLoadedItem |> page.Add

            let hasMore = page.Count = pageSize

            self.RemoveAt(self.Count - 1)
            self.AddRange(page)

            if hasMore then
                createLoadingItem () |> self.Add

        | _ -> ()

and LazyLoadableItem<'a>(data: Loadable<'a>, dataSource: LazyDataSource<'a>) =
    member _.IsLoading =
        match data with
        | Loadable.Loading -> true
        | Loadable.Loaded _ -> false

    member self.Data =
        if self.IsLoading then
            Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () -> dataSource.LoadNextPage())
            |> ignore

        data

let dataSource = LazyDataSource(commits, 20)

module ShapePath =
    let pathDataRightToBottom = "M 0,10 L 5,10 A 5 5 0 0 1 10 15 L 10 20" // right to bottom
    let pathDataTopToLeft = "M 0 10 L 5 10 A 5 5 0 0 0 10 5 L 10 0" // top to left
    let pathDataTopToRight = "M 20 10 L 14 10 A 5 5 0 0 1 10 5 L 10 0" // top to right
    let pathDataLeftToBottom = "M 20 10 L 15 10 A 5 5 0 0 0 10 15 L 10 20" // left to bottom
    let pathDataHorizontal = "M 0 10 L 20 10" // horizontal
    let pathDataVertical = "M 10 0 L 10 20" // vertical
    let pathDataHalfHorizontalRight = "M 10 10 L 20 10"
    let pathDataHalfHorizontalLeft = "M 0 10 L 10 10"
    let pathDataHalfVerticalTop = "M 10 00 L 10 10"
    let pathDataHalfVerticalBottom = "M 10 10 L 10 20"

    let createPath (pathData: string) targetIndex index =
        Path.create
            [
                Path.data pathData
                Path.width 20.0
                Path.height 20.0
                Path.stroke (getBrushFromIndex targetIndex)
                Path.strokeThickness 2.0
                Canvas.left (float index * 20.0)
            ]
        :> IView


    let createFromNode index (graphNode: GraphNode) =
        graphNode.Shapes
        |> List.map (
            function
            | GraphShape.HorizontalRightToBottom(GraphNodeReference.TargetColumnIndex target) ->
                createPath pathDataRightToBottom target index
            | GraphShape.HorizontalLeftToBottom(GraphNodeReference.TargetColumnIndex target) ->
                createPath pathDataLeftToBottom target index
            | GraphShape.VerticalTopToLeft(GraphNodeReference.TargetColumnIndex target) ->
                createPath pathDataTopToLeft target index
            | GraphShape.VerticalTopToRight(GraphNodeReference.TargetColumnIndex target) ->
                createPath pathDataTopToLeft target index
            | GraphShape.Horizontal(GraphNodeReference.TargetColumnIndex target) ->
                createPath pathDataHorizontal target index
            | GraphShape.Vertical -> createPath pathDataVertical index index
            | GraphShape.HorizontalRight(GraphNodeReference.TargetColumnIndex target) ->
                createPath pathDataHalfHorizontalRight target index
            | GraphShape.HorizontalLeft(GraphNodeReference.TargetColumnIndex target) ->
                createPath pathDataHalfHorizontalLeft target index
            | GraphShape.End -> createPath pathDataHalfVerticalTop index index
            | GraphShape.Start -> createPath pathDataHalfVerticalBottom index index
        )

let view selectedTab dispatch =


    DockPanel.create
        [
            DockPanel.lastChildFill true

            DockPanel.children
                [
                    Button.create
                        [
                            Button.dock Dock.Top
                            Button.content "Add"
                            Button.onClick (fun _ -> FuGit.Features.Main.Msg.SelectTab "aaaa" |> dispatch)
                        ]

                    // ScrollViewer.create
                    //     [
                    //         ScrollViewer.content (
                    //             ItemsRepeater.create
                    //                 [
                    //                     ItemsRepeater.dataItems dataSource
                    //                     ItemsRepeater.itemTemplate (
                    //                         DataTemplateView<_>.create (fun (data:LazyLoadableItem<string>) ->
                    //                             DockPanel.create
                    //                                 [
                    //                                     DockPanel.lastChildFill false
                    //                                     DockPanel.children [
                    //                                         TextBlock.create [
                    //                                             match data.Data with
                    //                                             | Loadable.Loaded (msg) -> TextBlock.text msg
                    //                                             | _ -> TextBlock.text "Loading"
                    //                                         ]
                    //                                     ]
                    //                                 ]
                    //                         )
                    //                     )
                    //                 ]
                    //         )

                    //     ]
                    DataGrid.create
                        [
                            DataGrid.dock Dock.Top
                            DataGrid.isReadOnly true
                            DataGrid.items dataSource
                            DataGrid.canUserResizeColumns true
                            DataGrid.gridLinesVisibility DataGridGridLinesVisibility.None
                            DataGrid.rowHeight 20.0

                            DataGrid.columns
                                [

                                    DataGridTemplateColumn.create
                                        [
                                            DataGridTemplateColumn.header "Graph"
                                            DataGridTemplateColumn.cellStyleClasses [ "graphCell" ]
                                            DataGridTemplateColumn.cellTemplate (
                                                DataTemplateView<_>.create (fun
                                                                                (lazyItem:
                                                                                    LazyLoadableItem<Commit *
                                                                                    GraphNode option list>) ->
                                                    match lazyItem.Data with
                                                    | Loadable.Loaded(_commit, graphNodes) ->
                                                        let shapeSize = 20.0

                                                        Canvas.create
                                                            [
                                                                Canvas.width (shapeSize * float graphNodes.Length)
                                                                Canvas.children (
                                                                    graphNodes
                                                                    |> List.indexed
                                                                    |> List.collect (fun (index, graphNode) ->
                                                                        match graphNode with
                                                                        | Some graphNode ->
                                                                            ShapePath.createFromNode index graphNode
                                                                        | None -> []
                                                                    )
                                                                )
                                                            ]
                                                        :> IView
                                                    | Loadable.Loading -> TextBlock.create [ TextBlock.text "Graph" ]
                                                )
                                            )
                                        ]

                                    DataGridTemplateColumn.create
                                        [
                                            DataGridTemplateColumn.header "Message"
                                            DataGridTemplateColumn.cellStyleClasses [ "graphCell" ]
                                            DataGridTemplateColumn.cellTemplate (
                                                DataTemplateView<_>.create (fun
                                                                                (lazyItem:
                                                                                    LazyLoadableItem<Commit *
                                                                                    GraphNode option list>) ->
                                                    TextBlock.create
                                                        [
                                                            match lazyItem.Data with
                                                            | Loadable.Loaded(commit, _) ->
                                                                TextBlock.text commit.MessageShort
                                                            | _ -> TextBlock.text ""

                                                        ]
                                                )
                                            )
                                        ]


                                ]
                        ]
                ]


        ]
// )