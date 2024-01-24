namespace FuGit

open Elmish
open Avalonia.FuncUI
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.DSL
open System.Threading.Tasks

module Counter =
    open Avalonia.Controls
    open Avalonia.Layout

    type State = { repository: LibGit2Sharp.Repository option }

    let init () = { repository = None }, Cmd.none

    [<RequireQualifiedAccess>]
    type Msg =
        | Open
        | Opened of LibGit2Sharp.Repository

    module Cmds =
        let openRepo (mainWindow: Avalonia.FuncUI.Hosts.HostWindow) =
            Cmd.ofEffect (fun dispatch ->
                let topLevel = TopLevel.GetTopLevel(mainWindow)

                let t =
                    task {
                        let options =
                            Avalonia.Platform.Storage.FolderPickerOpenOptions(
                                Title = "Open Git Repo",
                                AllowMultiple = false
                            )

                        let! selectedFolders = topLevel.StorageProvider.OpenFolderPickerAsync options

                        match selectedFolders |> Seq.tryHead with
                        | Some folder ->
                            let repo = new LibGit2Sharp.Repository(folder.Path.LocalPath)

                            // .Discover(folder.Path.LocalPath)
                            // let files = repo.RetrieveStatus(  LibGit2Sharp.StageOptions(IncludeIgnored))
                            Msg.Opened repo |> dispatch
                        | None -> ()
                        // dialog.Directory <- System.IO.Directory.GetCurrentDirectory()
                        // dialog.Title <- "Open Git Repo"
                        // dialog.ShowAsync(topLevel).ContinueWith(fun task ->
                        //     match task.Result with
                        //     | null -> ()
                        //     | result ->
                        //         match result.Length with
                        //         | 0 -> ()
                        //         | _ ->
                        //             let path = result.[0]
                        //             let repo = Git.Repository.Open(path)
                        //             let state = { count = 0 }
                        //             let msg = Open
                        //             dispatch msg
                        // ) |> ignore

                        ()
                    }

                t.Start()
            )


    let update (mainWindow: Avalonia.FuncUI.Hosts.HostWindow) (msg: Msg) (state: State) : State * Cmd<Msg> =
        match msg with
        | Msg.Open ->

            state, FuGit.Features.Clone.View.Cmds.openCloneView mainWindow
        // state, Cmds.openRepo mainWindow
        | Msg.Opened repo -> { state with repository = Some repo }, Cmd.none

    let view (state: State) (dispatch) =
        DockPanel.create
            [
                DockPanel.lastChildFill true
                DockPanel.children
                    [
                        Button.create
                            [
                                Button.dock Dock.Bottom
                                Button.onClick (fun _ -> Msg.Open |> dispatch)
                                Button.content "open"
                                Button.horizontalAlignment HorizontalAlignment.Stretch
                            ]
                        ScrollViewer.create
                            [
                                ScrollViewer.dock Dock.Left
                                ScrollViewer.content (
                                    StackPanel.create
                                        [
                                            Button.dock Dock.Left

                                            StackPanel.children
                                                [


                                                    TextBlock.create [ TextBlock.text "Remotes" ]

                                                    yield!
                                                        state.repository
                                                        |> Option.map (fun repo ->
                                                            repo.Network.Remotes
                                                            |> Seq.map (fun remote ->
                                                                TextBlock.create [ TextBlock.text remote.Url ]
                                                                :> IView
                                                            )
                                                        )
                                                        |> Option.defaultValue Seq.empty

                                                    TextBlock.create [ TextBlock.text "Branches" ]

                                                    yield!
                                                        state.repository
                                                        |> Option.map (fun repo ->
                                                            repo.Branches
                                                            |> Seq.map (fun b ->
                                                                TextBlock.create [ TextBlock.text b.CanonicalName ]
                                                                :> IView
                                                            )
                                                        )
                                                        |> Option.defaultValue Seq.empty
                                                ]
                                        ]
                                )
                            ]

                        ListBox.create
                            [
                                match state.repository with
                                | Some repo -> ListBox.dataItems repo.Commits
                                | None -> ()
                            ]

                    // TextBlock.create [
                    //     TextBlock.dock Dock.Top
                    //     TextBlock.fontSize 48.0
                    //     TextBlock.verticalAlignment VerticalAlignment.Center
                    //     TextBlock.horizontalAlignment HorizontalAlignment.Center
                    //     TextBlock.text (string state.count)
                    // ]
                    ]
            ]