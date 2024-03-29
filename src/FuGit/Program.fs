﻿namespace FuGit

open Avalonia
open Avalonia.Controls
open Avalonia.Themes.Fluent
open Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.Controls.ApplicationLifetimes
open FuGit.Features

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "FuGit"
        base.Icon <- WindowIcon("icon.ico")
        base.Height <- 400.0
        base.Width <- 400.0

        //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true
        Elmish.Program.mkProgram
            FuGit.Features.App.State.init
            FuGit.Features.App.State.update
            FuGit.Features.App.View.view
        |> Program.withHost this
        |> Program.withConsoleTrace
        |> Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let mainWindow = MainWindow()
            desktopLifetime.MainWindow <- mainWindow

            desktopLifetime.ShutdownRequested.Add(fun arg ->
                match CloseAwaiter.close () with
                | None -> ()
                | Some delayTask ->
                    arg.Cancel <- true

                    async {
                        do! delayTask |> Async.AwaitTask
                        desktopLifetime.Shutdown()
                    }
                    |> Async.StartImmediate
                    |> ignore
            )

        | _ -> ()

module Program =

    [<EntryPoint>]
    let main (args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)