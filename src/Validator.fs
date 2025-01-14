module Validator

#if INTERACTIVE
#r "nuget: FSToolkit.ErrorHandling"
#endif
open FsToolkit.ErrorHandling

// Активный паттерн
let (|CanBeFloat|_|) (input: string) = Parser.tryParseFloat input

type ErrorType =
    | InvalidData of name: string * value: string
    | LineFormat of expected: string * value: string

// Функция для преобразования ошибки в строку
let errorToString error =
    match error with
    | InvalidData(name, value) -> sprintf "Parse error at %s: %s" name value
    | LineFormat(expected, value) -> sprintf "Wrong line format, expected '%s', got '%s'" expected value

// Функция для проверки, что строка является допустимым числом с плавающей запятой
let validateFloat (s: string) =
    match s with
    | CanBeFloat s -> Ok s
    | _ -> Error(InvalidData("Float", s))

let splitLine (s: string) =
    match s.Split() with
    | [| left; right |] -> Ok(left, right)
    | _ -> Error([ LineFormat("Float Float", s) ])

// Функция для парсинга и валидации двух строковых аргументов как чисел с плавающей запятой
let parseFloatArgs (left: string) (right: string) =
    validation {
        let! validLeft = validateFloat left
        and! validRight = validateFloat right
        return (validLeft, validRight)
    }

let parseAndValidate s =
    result {
        let! (left, right) = splitLine s
        return! parseFloatArgs left right
    }