module FuGit.Features.Clone.View

open Elmish
open Avalonia.FuncUI
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open System.Threading.Tasks

open Avalonia.Controls
open Avalonia.Layout

let view (dispatch) =
    DockPanel.create
        [
            DockPanel.lastChildFill true
            DockPanel.children
                [
                    StackPanel.create
                        [
                            StackPanel.children
                                [
                                    TextBlock.create
                                        [
                                            TextBlock.text "Clone"
                                            TextBlock.fontSize 20.
                                        ]
                                    TextBlock.create
                                        [
                                            TextBlock.text "Clone a repository"
                                            TextBlock.fontSize 12.
                                        ]
                                ]
                        ]
                ]
        ]


module Cmds =
    let openCloneView (mainWindow: Avalonia.FuncUI.Hosts.HostWindow) =
        Cmd.ofEffect (fun dispatch ->
            task {
                let w =
                    HostWindow(
                        Title = "Clone",
                        Width = 800,
                        Height = 600
                    // Content = view dispatch

                    )

                (w :> IViewHost).Update(Some <| view dispatch)

                do! w.ShowDialog(mainWindow)
            }
            |> Async.AwaitTask
            |> Async.StartImmediate
        )


// let topLevel = TopLevel.GetTopLevel(mainWindow)
// Hosts.HostControl.create
//     (fun dispatch ->
//         view dispatch
//     )
// ()

// let window =
//     HostWindow.create
//         [
//             HostWindow.Title "Clone"
//             HostWindow.Width 800.
//             HostWindow.Height 600.
//         ]
//         (fun dispatch ->
//             view dispatch
//         )

// HostWindow.Run window