namespace FuGit.Features.Main

type MainModel =
    {
        Tabs: string list
        SelectedTab: string
    }

[<RequireQualifiedAccess>]
type Msg = | SelectTab of string