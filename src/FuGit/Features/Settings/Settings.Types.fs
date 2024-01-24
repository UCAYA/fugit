namespace FuGit.Features.Settings

open FuGit

type Settings = { Repositories: Repository array }

module Settings =
    let defaultSettings =
        { Repositories = Array.empty }

type LoadedModel = { Settings: Settings }

type Model = LoadedModel Loadable

type LoadSettingsErrors =
    | SettingsFileNotFound of string
    | SettingsFileParseError of string

[<RequireQualifiedAccess>]
type Msg =
    | UpdateSettings of Settings
    | StateLoaded of Result<Settings, LoadSettingsErrors>

[<RequireQualifiedAccess>]
type OutMsg =
    | SettingsLoaded of Result<Settings, LoadSettingsErrors>
