[<AutoOpen>]
module Resources

open Avalonia
open Avalonia.Styling
open Avalonia.Controls


[<RequireQualifiedAccess>]
module ControlThemes =

    let dataGridCell =
        lazy
            (match Application.Current.TryFindResource "DataGridCell" with
             | true, theme -> theme :?> ControlTheme
             | false, _ -> failwithf "Could not find theme 'DataGridCell'")