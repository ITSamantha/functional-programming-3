module Parser

// Попытка преобразования в float
let tryParseFloat (input: string) =
    match System.Double.TryParse(
            input,
            System.Globalization.NumberStyles.Number,
            System.Globalization.CultureInfo.InvariantCulture
        ) with
    | true, value -> Some value
    | _ -> None

// Попытка преобразования в int
let tryParseInt16 (input: string) =
    match System.Int16.TryParse(
            input,
            System.Globalization.NumberStyles.Number,
            System.Globalization.CultureInfo.InvariantCulture
        ) with
    | true, value -> Some value
    | _ -> None