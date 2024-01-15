module FuGit.Features.Main.State

open Elmish
open FuGit.Features.Main

let init () =
    {
        Tabs =
            [
                "Counter"
                "About"
                "About1"
                "About2"
                "About3"
                "About4"
                "About5"
                "About6"
                "About7"
                "About8"
            ]
        SelectedTab = "Counter"
    },
    Cmd.none

let update (msg: Msg) (state: MainModel) =
    match msg with
    | Msg.SelectTab tab -> { state with SelectedTab = tab }, Cmd.none