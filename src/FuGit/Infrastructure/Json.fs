module Json

open Thoth.Json.Net

let jsonExtra =
    Extra.empty

let encoderWith<'T> extra =
    Encode.Auto.generateEncoderCached<'T> (caseStrategy = CaseStrategy.CamelCase, extra = extra)

let decoderWith<'T> extra =
    Decode.Auto.generateDecoderCached<'T> (caseStrategy = CaseStrategy.CamelCase, extra = extra)

let encoder<'T> = encoderWith<'T> jsonExtra
let decoder<'T> = decoderWith<'T> jsonExtra

let toString x =
    x
    |> encoder
    |> Encode.toString 0

let fromString<'T> str =
    str |> Decode.fromString decoder<'T>
