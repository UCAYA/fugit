namespace FuGit.Features.Settings


type Settings = { Repositories: string [] }

module Settings =
    let defaultSettings =
        { Repositories = [| |] }

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
