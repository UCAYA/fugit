module FuGit.Features.Main.View

open Elmish
open Avalonia
open Avalonia.FuncUI
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open System.Threading.Tasks
open FuGit.Features
open FuGit.Features.Main

open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout

let tabHeaderView isSelected (tab: string) dispatch =
    RadioButton.create
        [
            RadioButton.content tab
            RadioButton.isChecked (Some isSelected)
            RadioButton.onIsCheckedChanged (
                (fun (e: Interactivity.RoutedEventArgs) ->
                    let isChecked = (e.Source :?> RadioButton).IsChecked |> Option.ofNullable

                    if isChecked = Some true && not isSelected then
                        Msg.SelectTab tab |> dispatch
                ),
                SubPatchOptions.OnChangeOf isSelected
            )
        ]

let view mainModel (dispatch) =
    DockPanel.create
        [
            DockPanel.lastChildFill true
            DockPanel.children
                [
                    Menu.create
                        [
                            Menu.dock Dock.Top
                            Menu.viewItems
                                [
                                    MenuItem.create
                                        [
                                            MenuItem.header "File"
                                            MenuItem.viewItems
                                                [
                                                    MenuItem.create
                                                        [
                                                            MenuItem.header "Open Repository"
                                                        // MenuItem.viewItems (
                                                        //     mainModel.Repositories
                                                        //     |> List.map (fun repo ->
                                                        //         MenuItem.create [
                                                        //             MenuItem.header repo.Name
                                                        //             MenuItem.command (fun _ -> Msg.Opened repo |> dispatch)
                                                        //         ]
                                                        //     )
                                                        // )
                                                        ]
                                                ]
                                        ]
                                ]
                        ]
                    ScrollViewer.create
                        [
                            ScrollViewer.dock Dock.Top
                            ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Disabled
                            ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                            ScrollViewer.content (
                                StackPanel.create
                                    [
                                        StackPanel.orientation Orientation.Horizontal
                                        StackPanel.children (
                                            mainModel.Tabs
                                            |> List.map (fun tab ->
                                                tabHeaderView (tab = mainModel.SelectedTab) tab dispatch
                                            )
                                        )
                                    ]
                            )
                        ]

                    Graph.View.view mainModel.SelectedTab dispatch
                ]
        ]