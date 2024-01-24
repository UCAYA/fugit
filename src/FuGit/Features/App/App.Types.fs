namespace FuGit.Features.App

open FuGit.Features

[<RequireQualifiedAccess>]
type AppMsg =
    | State of AppState.Msg
    | Settings of Settings.Msg
    | Main of Main.Msg

type LoadingAppModel =
    { Settings: Settings.Model
      State: AppState.Model }

type LoadedAppModel =
    { Main: Main.MainModel
      Settings: Settings.LoadedModel
      State: AppState.LoadedModel
    }

[<RequireQualifiedAccess>]
type AppModel =
    | Loading of LoadingAppModel
    | Loaded of LoadedAppModel