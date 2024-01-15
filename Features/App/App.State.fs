module FuGit.Features.App.State

open FuGit.Features
open Elmish

let init () =
    // let mainModel, mainCmd = Main.State.init ()

    // let cmd = [ Cmd.map AppMsg.MainMsg mainCmd ] |> Cmd.batch
    // { Main = mainModel }, cmd

    let stateModel, stateCmd = AppState.State.init ()
    let settingsModel, settingsCmd = Settings.State.init ()

    let model =
        {   Settings = settingsModel
            State = stateModel }
        |> AppModel.Loading

    let cmd =
        [   settingsCmd |> Cmd.map AppMsg.Settings
            stateCmd |> Cmd.map AppMsg.State  ]
        |> Cmd.batch

    model, cmd

let initLoadedAppModel (settingsModel:Settings.LoadedModel) (stateModel:AppState.LoadedModel) =
    let mainModel, mainCmd = Main.State.init ()

    let cmd = mainCmd |> Cmd.map AppMsg.Main

    { Main = mainModel
      Settings = settingsModel
      State = stateModel }, cmd

let updateAppLoading msg (model:LoadingAppModel) =

    let trySwitchToLoadedModel (model:LoadingAppModel, loadingCmd) =
        match model.Settings, model.State with
        | Loadable.Loaded settingsModel, Loadable.Loaded stateModel ->
            let loadedModel, loadedCmd = initLoadedAppModel settingsModel stateModel
            let cmds =
                [loadingCmd; loadedCmd] |> Cmd.batch

            loadedModel |> AppModel.Loaded, cmds

        | _ -> model |> AppModel.Loading, Cmd.none

    match msg with
    | AppMsg.Settings msg ->
        let settingsModel, settingsCmd = Settings.State.update msg model.Settings
        { model with Settings = settingsModel }, Cmd.map AppMsg.Settings settingsCmd
    | AppMsg.State msg ->
        let stateModel, stateCmd = AppState.State.update msg model.State
        { model with State = stateModel }, Cmd.map AppMsg.State stateCmd
    | _ ->
        // Other msgs are not handled for loading state
        model, Cmd.none

    |> trySwitchToLoadedModel


let updateAppLoaded msg model =
    match msg with
    | AppMsg.Main msg ->
        let mainModel, mainCmd = Main.State.update msg model.Main
        { model with Main = mainModel }, mainCmd |> Cmd.map AppMsg.Main

    | AppMsg.Settings msg ->
        let settingsModel, settingsCmd = Settings.State.updateLoadedModel msg model.Settings
        { model with Settings = settingsModel }, settingsCmd |> Cmd.map AppMsg.Settings
    | AppMsg.State msg ->
        let stateModel, stateCmd = AppState.State.updateLoadedModel msg model.State
        { model with State = stateModel }, stateCmd |> Cmd.map AppMsg.State


let update msg model =
    match model with
    | AppModel.Loading loadingAppModel -> updateAppLoading msg loadingAppModel
    | AppModel.Loaded loadedAppModel ->
        let loadedAppModel, cmd = updateAppLoaded msg loadedAppModel
        loadedAppModel |> AppModel.Loaded, cmd
