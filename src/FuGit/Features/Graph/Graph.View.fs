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
    |> Seq.map (fun x -> x |> unbox<Commit * GraphNode list> |> (fun (a, _b) -> a.MessageShort))
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


// type IncrementalLoadingList<'a> () =
//     inherit AvaloniaList<'a>()

//override Item
// let commits =
//     [|
//         {| Commit = "aaaa" |}
//         {| Commit = "bbb" |}
//     |]

// type Person(name, age, male) =
//     member val Name = name with get, set
//     member val Age = age with get, set
//     member val IsMale = male with get, set

// let col = ObservableCollection()
// col.Add()

// let colView = Avalonia.Collections.AvaloniaList()

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

// type Template(update) =

//     let hostControl = HostControl()
//     let viewHost: IViewHost = unbox hostControl

//     interface Avalonia.Controls.Templates.IDataTemplate with
//         member this.Match _data = true

//         member this.Build data =
//             System.Diagnostics.Debug.WriteLine($"Build {data}")
//             viewHost.Update(update data)
//             hostControl

//     static member create(update: obj -> IView<_> option) =
//         let upd data =
//             update data |> Option.map (unbox<IView>)

//         Template(upd)

//     static member create(update: obj -> IView option) = Template(update)


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

            // let rawItems = ResizeArray<_>(pageSize)
            while page.Count < pageSize && iterator.MoveNext() do
                // printfn "push %A " (iterator.Current |> unbox<Commit * GraphNode list> |> fun (a, _b) -> a.MessageShort)
                // (iterator.Current |> unbox<Commit * GraphNode list> |> fun (a, _b) -> a.MessageShort) |> createLoadedItem |> page.Add
                iterator.Current |> createLoadedItem |> page.Add
            // printfn "push %A " (iterator.Current |> unbox<Commit * GraphNode list> |> fun (a, _b) -> a.MessageShort)
            // iterator.Current |> unbox<Commit * GraphNode list> |> fun (a, _b) -> a.MessageShort |> rawItems.Add

            let hasMore = page.Count = pageSize
            // match page |> Seq.tryHead  with
            // | Some head ->
            //     self[self.Count - 1] <- head
            //     page.RemoveAt(0)
            //     self.AddRange(page)

            //     if hasMore then
            //         createLoadingItem() |> self.Add
            // | _ -> ()

            self.RemoveAt(self.Count - 1)
            self.AddRange(page)

            if hasMore then
                createLoadingItem () |> self.Add

        // self[0] <-
        // self.InsertRange(self.Count - 1, page)

        // if page.Count <> pageCount then
        //     self.RemoveAt(self.Count - 1)


        | _ -> ()

and LazyLoadableItem<'a>(data: Loadable<'a>, dataSource: LazyDataSource<'a>) =
    member _.IsLoading =
        match data with
        | Loadable.Loading -> true
        | Loadable.Loaded _ -> false

    member self.Data =
        if self.IsLoading then
            // DispatchQueue.main.async (fun () -> dataSource.LoadNextPage())
            Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () -> dataSource.LoadNextPage())
            |> ignore
        // dataSource.LoadNextPage()
        data
// public class CustomDataTemplate : IDataTemplate
// {
//     [DataType]
//     public Type? FancyDataType { get; set; }

//     [Content]
//     [TemplateContent]
//     public object? Content { get; set; }

//     public bool Match(object? data) => FancyDataType?.IsInstanceOfType(data) ?? true;

//     public Control? Build(object? data) => TemplateContent.Load(Content)?.Result;
// }


// let repo = new Repository()

// let items = Array.init 100000 (fun i -> Person("John", i, true))

// // let data =
// //     ObservableCollection
// //         [

// //             Person("Jane", 21, false)
// //             Person("Bob", 22, true)
// //         ]

// let template =
//     Template.create (fun (data: obj) ->

//         TextBlock.create [ TextBlock.text $"data {data}" ] |> Some
//     )
// |> Seq.map ( fun (c,_) -> c.MessageShort)
let dataSource = LazyDataSource(commits, 2)

let view selectedTab dispatch =

    //     let data = ctx.useState (
    //         ObservableCollection [
    //             Person("John", 20, true)
    //             Person("Jane", 21, false)
    //             Person("Bob", 22, true)
    //         ]
    //     )
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

                                    // DataGridTemplateColumn.create
                                    //     [
                                    //         DataGridTemplateColumn.header "Message"
                                    //         DataGridTemplateColumn.cellTemplate (
                                    //             DataTemplateView<_>.create (
                                    //                 fun  (data:LazyLoadableItem<string>) ->
                                    //                     TextBlock.create [
                                    //                         match data.Data with
                                    //                         | Loadable.Loaded (msg) ->
                                    //                             TextBlock.text msg
                                    //                         | _ -> TextBlock.text "Loading"
                                    //                     ]
                                    //             )
                                    //         )
                                    //     ]

                                    // DataGridTemplateColumn.create
                                    //     [
                                    //         DataGridTemplateColumn.header "Name"
                                    //         DataGridTemplateColumn.cellTemplate (
                                    //             DataTemplateView<_>.create (fun
                                    //                                             (data:
                                    //                                                 LazyLoadableItem<Commit *
                                    //                                                 GraphNode list>) ->
                                    //                 TextBlock.create
                                    //                     [
                                    //                         match data.Data with
                                    //                         | Loadable.Loaded(commit, _) ->
                                    //                             TextBlock.text commit.MessageShort
                                    //                         | _ -> TextBlock.text "Loading"
                                    //                     ]
                                    //             )
                                    //         )
                                    //     ]

                                    DataGridTemplateColumn.create
                                        [
                                            DataGridTemplateColumn.header "Graph"
                                            // DataGridTemplateColumn.cellTheme ControlThemes.dataGridCell.Value
                                            DataGridTemplateColumn.cellStyleClasses [ "graphCell" ]
                                            DataGridTemplateColumn.cellTemplate (
                                                DataTemplateView<_>.create (fun
                                                                                (data:
                                                                                    LazyLoadableItem<Commit *
                                                                                    GraphNode list>) ->
                                                    match data.Data with
                                                    | Loadable.Loaded(commit, graphNodes) ->
                                                        let shapeSize = 20.0

                                                        Canvas.create
                                                            [
                                                                Canvas.width (shapeSize * float graphNodes.Length)
                                                                Canvas.children (
                                                                    graphNodes
                                                                    |> List.map (fun node ->

                                                                        Path.create
                                                                            [
                                                                                Path.data
                                                                                    "M 0,10 L 5,10 A 5 5 0 0 1 10 15 L 10 20"
                                                                                Path.width 20.0
                                                                                Path.height 20.0
                                                                                Path.stroke (getBrushFromIndex 0)
                                                                                Path.strokeThickness 2.0
                                                                            ]
                                                                    )
                                                                )
                                                            ]
                                                        :> IView
                                                    | Loadable.Loading -> TextBlock.create [ TextBlock.text "Graph" ]
                                                )
                                            )
                                        ]

                                ]
                        ]
                ]


        ]
// )