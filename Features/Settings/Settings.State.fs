module FuGit.Features.Settings.State

open Elmish
open System
open System.IO
open System.Threading
open FsToolkit.ErrorHandling

let defaultSettingsPath =
    System.IO.Path.Combine
        (System.Environment.GetFolderPath System.Environment.SpecialFolder.ApplicationData, "fugit", "settings")

let saveLock = obj

module Cmds =
    let load () =
        Cmd.ofEffect (fun dispatch ->
            asyncResult {
                if not (File.Exists defaultSettingsPath) then
                    return! SettingsFileNotFound defaultSettingsPath |> Result.Error
                else
                    let! raw =
                        defaultSettingsPath
                        |> File.ReadAllTextAsync
                    return!
                        raw
                        |> Json.fromString<Settings>
                        |> Result.mapError (fun err -> SettingsFileParseError err)

            }
            |> Async.map (Msg.StateLoaded >> dispatch)
            |> Async.StartImmediate
        )

    let persist appState =
        Cmd.ofEffect (fun _ ->
            CloseAwaiter.tryStartDelayTask (fun () ->
                let save () =
                    let raw = appState |> Json.toString
                    File.WriteAllTextAsync(defaultSettingsPath, raw) |> Async.AwaitTask

                lock saveLock save
            ) |> ignore
        )

let init () =
    Loadable.Loading,
    Cmds.load ()

let updateLoadedModel msg model =
    match msg with
    | Msg.UpdateSettings settings ->
        { model with Settings = settings}, Cmd.none

    | Msg.StateLoaded _loadedResult ->
        // Handled by loading state update function
        model, Cmd.none


let update msg model =
    match model with
    | Loadable.Loading ->
        match msg with
        | Msg.StateLoaded loadedResult ->
            match loadedResult with
            | Error _err -> { Settings = Settings.defaultSettings } |> Loadable.Loaded, Cmd.none
            | Ok appState -> { Settings = appState } |> Loadable.Loaded , Cmd.none
        | _ ->
            // No other messages should be sent to the Loading state
            model, Cmd.none

    | Loadable.Loaded loadedModel ->
        let newLoadedModel, cmd = loadedModel |> updateLoadedModel msg

        newLoadedModel |> Loadable.Loaded, cmd
