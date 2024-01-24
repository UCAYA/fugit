module FuGit.Features.AppState.State

open Elmish
open System.IO
open FsToolkit.ErrorHandling

let defaultStatePath =
    System.IO.Path.Combine
        (System.Environment.GetFolderPath System.Environment.SpecialFolder.ApplicationData, "fugit", "state")

let saveLock = obj

module Cmds =
    let load () =
        Cmd.ofEffect (fun dispatch ->
            asyncResult {
                if not (File.Exists defaultStatePath) then
                    return! StateFileNotFound defaultStatePath |> Result.Error
                else
                    let! raw =
                        defaultStatePath
                        |> File.ReadAllTextAsync
                    return!
                        raw
                        |> Json.fromString<AppState>
                        |> Result.mapError (fun err -> StateFileParseError err)

            }
            |> Async.map (Msg.StateLoaded >> dispatch)
            |> Async.StartImmediate
        )

    let persist appState =
        Cmd.ofEffect (fun _ ->
            CloseAwaiter.tryStartDelayTask (fun () ->
                let save () =
                    let raw = appState |> Json.toString
                    File.WriteAllTextAsync(defaultStatePath, raw) |> Async.AwaitTask

                lock saveLock save
            ) |> ignore
        )

let init () =
    Loadable.Loading,
    Cmds.load ()


let defaultAppState = { OpenedTab = "Settings" }


let updateLoadedModel msg model =
    match msg with
    | Msg.UpdateState appState ->
        { model with State = appState}, Cmd.none

    | Msg.StateLoaded _loadedResult ->
        // Handled by loading state update function
        model, Cmd.none

let update msg model =
    match model with
    | Loadable.Loading ->
        match msg with
        | Msg.StateLoaded loadedResult ->
            match loadedResult with
            | Error _err -> { State = defaultAppState } |> Loadable.Loaded, Cmd.none
            | Ok appState -> { State = appState } |> Loadable.Loaded , Cmd.none
        | _ ->
            // No other messages should be sent to the Loading state
            model, Cmd.none

    | Loadable.Loaded loadedModel ->
        let newLoadedModel, cmd = loadedModel |> updateLoadedModel msg

        newLoadedModel |> Loadable.Loaded, cmd