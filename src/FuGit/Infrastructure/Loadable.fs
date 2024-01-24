[<AutoOpen>]
module LoadableModule

[<RequireQualifiedAccess>]
type Loadable<'T> =
    | Loading
    | Loaded of 'T
