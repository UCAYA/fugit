[<AutoOpen>]
module Polyglot


module Configuration =
    // The string that separates the different phrase possibilities.
    let delimiter = "||||"

    let russianPluralGroups (nf: float) =
        let n = int nf
        let lastTwo = n % 100
        let end' = lastTwo % 10

        if lastTwo <> 11 && end' <> 1 then
            0
        elif 2. <= end' && end' <= 4 && not (lastTwo >= 12 && lastTwo <= 14) then
            1
        else
            2

    type PluralTypes =
        | Arabic
        | BosnianSerbian
        | Chinese
        | Croatian
        | German
        | French
        | Russian
        | Lithuanian
        | Czech
        | Polish
        | Icelandic
        | Slovenian
        | Romanian

    // Mapping from pluralization group to individual language codes/locales.
    // Will look up based on exact match, if not found and it's a locale will parse the locale
    // for language code, and if that does not exist will default to 'en'
    let defaultPluralTypeToLanguages =
        [
            Arabic, [ "ar" ]
            BosnianSerbian,
            [
                "bs-Latn-BA"
                "bs-Cyrl-BA"
                "srl-RS"
                "sr-RS"
            ]
            Chinese,
            [
                "id"
                "id-ID"
                "ja"
                "ko"
                "ko-KR"
                "lo"
                "ms"
                "th"
                "th-TH"
                "zh"
            ]
            Croatian,
            [
                "hr"
                "hr-HR"
            ]
            German,
            [
                "fa"
                "da"
                "de"
                "en"
                "es"
                "fi"
                "el"
                "he"
                "hi-IN"
                "hu"
                "hu-HU"
                "it"
                "nl"
                "no"
                "pt"
                "sv"
                "tr"
            ]
            French,
            [
                "fr"
                "tl"
                "pt-br"
            ]
            Russian,
            [
                "ru"
                "ru-RU"
            ]
            Lithuanian, [ "lt" ]
            Czech,
            [
                "cs"
                "cs-CZ"
                "sk"
            ]
            Polish, [ "pl" ]
            Icelandic,
            [
                "is"
                "mk"
            ]
            Slovenian, [ "sl-SL" ]
            Romanian, [ "ro" ]
        ]
        |> Map.ofList


    // Mapping from pluralization group plural logic.
    let defaultPluralTypes =
        [
            Arabic,
            fun (nf) ->
                let n = int nf
                // http://www.arabeyes.org/Plural_Forms
                if n < 3 then
                    n
                else
                    let lastTwo = n % 100

                    if lastTwo >= 3 && lastTwo <= 10 then
                        3
                    elif lastTwo >= 11 then
                        4
                    else
                        5
            BosnianSerbian, russianPluralGroups
            Chinese, (fun _ -> 0)
            Croatian, russianPluralGroups
            French,
            fun (nf) ->
                let n = int nf

                if n >= 2 then
                    1
                else
                    0
            German,
            fun (nf) ->
                let n = int nf

                if n <> 1 then
                    1
                else
                    0
            Russian, russianPluralGroups
            Lithuanian,
            fun (nf) ->
                let n = int nf

                if n % 10 = 1 && n % 100 <> 11 then
                    0
                elif n % 10 >= 2 && n % 10 <= 9 && (n % 100 < 11 || n % 100 > 19) then
                    1
                else
                    2

            Czech,
            fun (nf) ->
                let n = int nf

                if n = 1 then
                    0
                elif n >= 2 && n <= 4 then
                    1
                else
                    2

            Polish,
            fun (nf) ->
                let n = int nf

                if n = 1 then
                    0
                else
                    let end' = n % 10

                    if 2 <= end' && end' <= 4 && (n % 100 < 10 || n % 100 >= 20) then
                        1
                    else
                        2

            Icelandic,
            fun (nf) ->
                let n = int nf

                if n % 10 <> 1 || n % 100 = 11 then
                    1
                else
                    0
            Slovenian,
            fun (nf) ->
                let n = int nf
                let lastTwo = n % 100

                if lastTwo = 1 then
                    0
                elif lastTwo = 2 then
                    1
                elif lastTwo = 3 || lastTwo = 4 then
                    2
                else
                    3

            Romanian,
            fun (nf) ->
                let n = int nf

                if n = 1 then
                    0
                else
                    let lastTwo = n % 100

                    if n = 0 || (lastTwo >= 2 && lastTwo <= 19) then
                        1
                    else
                        2
        ]
        |> Map.ofList

    type PluralRules =
        {
            pluralTypes: Map<PluralTypes, (float -> int)>
            pluralTypeToLanguages: Map<PluralTypes, string list>
        }

    let defaultPluralRules =
        {
            pluralTypes = defaultPluralTypes
            pluralTypeToLanguages = defaultPluralTypeToLanguages
        }

    let langToTypeMap (mapping: Map<PluralTypes, string list>) =
        (Map.empty, mapping)
        ||> Map.fold (fun acc pluralType locales ->
            (acc, locales) ||> List.fold (fun acc locale -> acc.Add(locale, pluralType))
        )

    let pluralTypeName pluralRules (locale: string) =
        let langToPluralType = langToTypeMap (pluralRules.pluralTypeToLanguages)

        langToPluralType
        |> Map.tryFind locale
        |> Option.orElseWith (fun () ->
            let localeSplit = locale.Split('-')
            langToPluralType |> Map.tryFind localeSplit[0]
        )
        |> Option.orElseWith (fun () -> langToPluralType |> Map.tryFind "en")

    let defaultTokenRegex =
        System.Text.RegularExpressions.Regex(@"%\{(.*?)\}", System.Text.RegularExpressions.RegexOptions.Compiled)

    let defaultReplace =
        fun
            (regex: System.Text.RegularExpressions.Regex)
            (str: string)
            (matchEvaluator: System.Text.RegularExpressions.MatchEvaluator) -> regex.Replace(str, matchEvaluator)

    type InterpolationOptions =
        {
            prefix: string
            suffix: string
        }

    let constructTokenRegex opts =
        let escape token =
            System.Text.RegularExpressions.Regex.Replace(token, @"[.*+?^${}()|[\]\\]", "\\$&")

        let prefix = opts |> Option.map (fun o -> o.prefix) |> Option.defaultValue "%{"
        let suffix = opts |> Option.map (fun o -> o.suffix) |> Option.defaultValue "}"

        if prefix = delimiter || suffix = delimiter then
            failwithf "The delimiter '%s' is reserved for pluralization" delimiter

        System.Text.RegularExpressions.Regex(
            $"{prefix |> escape}(.*?){suffix |> escape}",
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    let mutable cache = Map.empty

    let memoizedPluralTypeNameSelector (locale: string) pluralRules =
        match cache |> Map.tryFind locale with
        | Some pluralType -> pluralType
        | None ->
            let pluralType = pluralTypeName pluralRules locale
            cache <- cache |> Map.add locale pluralType
            pluralType

    [<Literal>]
    let SmartCountKey = "smart_count"

    [<RequireQualifiedAccess>]
    type Substitution =
        | SmartCount of int
        | Map of Map<string, obj>

    let (|SmartCount|_|) (substitutions: Map<string, obj>) =
        match substitutions |> Map.tryFind SmartCountKey with
        | Some(:? int as count) -> Some count
        | Some(:? float as count) -> Some(int count)
        | Some other ->
            try
                System.Convert.ToInt32 other |> Some
            with _ ->
                None
        | _ -> None

open Configuration

// ### transformPhrase(phrase, substitutions, locale)
//
// Takes a phrase string and transforms it by choosing the correct
// plural form and interpolating it.
//
//     transformPhrase('Hello, %{name}!', {name: 'Spike'});
//     // "Hello, Spike!"
//
// The correct plural form is selected if substitutions.smart_count
// is set. You can pass in a number instead of an Object as `substitutions`
// as a shortcut for `smart_count`.
//
//     transformPhrase('%{smart_count} new messages |||| 1 new message', {smart_count: 1}, 'en');
//     // "1 new message"
//
//     transformPhrase('%{smart_count} new messages |||| 1 new message', {smart_count: 2}, 'en');
//     // "2 new messages"
//
//     transformPhrase('%{smart_count} new messages |||| 1 new message', 5, 'en');
//     // "5 new messages"
//
// You should pass in a third argument, the locale, to specify the correct plural type.
// It defaults to `'en'` with 2 plural forms.
let transformPhrase (phrase: string) substitutions locale tokenRegex pluralRules replaceImplementation =

    let interpolationRegex = tokenRegex |> Option.defaultValue defaultTokenRegex
    let replace = replaceImplementation |> Option.defaultValue defaultReplace

    let options =
        match substitutions with
        | Substitution.Map map -> map
        | Substitution.SmartCount count -> Map.ofList [ SmartCountKey, count ]


    // Select plural form: based on a phrase text that contains `n`
    // plural forms separated by `delimiter`, a `locale`, and a `substitutions.smart_count`,
    // choose the correct plural form. This is only done if `count` is set.
    let result =
        match options with
        | SmartCount smartCount ->
            let pluralRulesOrDefault = pluralRules |> Option.defaultValue defaultPluralRules
            let texts = phrase.Split(delimiter)

            let bestLocale =
                locale
                |> Option.defaultValue (System.Globalization.CultureInfo.CurrentUICulture.Name)


            pluralRulesOrDefault
            |> memoizedPluralTypeNameSelector bestLocale
            |> Option.map (fun pluralType ->
                let pluralTypeWithCount =
                    pluralRulesOrDefault.pluralTypes
                    |> Map.tryFind pluralType
                    |> Option.map (fun pluralize -> pluralize smartCount)
                    |> Option.defaultValue 0

                let result =
                    texts |> Array.tryItem pluralTypeWithCount |> Option.defaultValue texts[0]


                result.Trim()
            )
            |> Option.defaultValue phrase
        | _ -> phrase

    let matchEvaluator =
        System.Text.RegularExpressions.MatchEvaluator(fun (m: System.Text.RegularExpressions.Match) ->
            let argument =
                m.Captures
                |> Seq.tryHead
                |> Option.bind (fun obj ->
                    let capture: System.Text.RegularExpressions.Capture = obj |> unbox

                    options |> Map.tryFind (capture.Value) |> Option.bind Option.ofObj
                )

            argument |> Option.map string |> Option.defaultValue m.Value
        )

    replace interpolationRegex result matchEvaluator

type Polyglot =
    static member transform(phrase, smartCount: int, ?locale, ?tokenRegex, ?pluralRules, ?replace) =
        transformPhrase phrase (Substitution.SmartCount smartCount) locale tokenRegex pluralRules replace

    static member transform(phrase, ?substitutions, ?locale, ?tokenRegex, ?pluralRules, ?replace) =
        transformPhrase
            phrase
            (Substitution.Map(substitutions |> Option.defaultValue Map.empty))
            locale
            tokenRegex
            pluralRules
            replace