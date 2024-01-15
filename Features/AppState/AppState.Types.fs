namespace FuGit.Features.AppState

open System.Threading

type AppState = { OpenedTab: string }

module AppStateState =
    let defaultAppState = { OpenedTab = "Settings" }

type LoadedModel = { State: AppState }
type Model = LoadedModel Loadable

type LoadStateErrors =
    | StateFileNotFound of string
    | StateFileParseError of string

[<RequireQualifiedAccess>]
type Msg =
    | UpdateState of AppState
    | StateLoaded of Result<AppState, LoadStateErrors>

[<RequireQualifiedAccess>]
type OutMsg =
    | StateLoaded of Result<AppState, LoadStateErrors>
