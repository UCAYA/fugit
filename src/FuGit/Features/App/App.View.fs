namespace FuGit.Features.App

open FuGit.Features

module View =
    open Avalonia
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Types
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Hosts
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.Layout


    let LoaderView =
        DockPanel.create
            [

            ]

    let view (model: AppModel) dispatch =

        match model with
        | AppModel.Loading _loadingModel ->
            DockPanel.create
                [
                    DockPanel.lastChildFill true
                    DockPanel.children
                        [
                            TextBlock.create [ TextBlock.text "Loading..." ]
                        ]
                ]

        | AppModel.Loaded loadedModel -> Main.View.view loadedModel.Main (AppMsg.Main >> dispatch)