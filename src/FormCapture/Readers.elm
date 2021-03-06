module FormCapture.Readers exposing
  ( readStringAt, readIntAt, readFloatAt, readBoolAt, readCustomAt, toJson )

{-| ## Read values captured from form input with type validation

# Reading Input Values
@docs readStringAt, readIntAt, readFloatAt, readBoolAt, readCustomAt

# Converting `FormInput` to JSON
@docs toJson
-}

import FormCapture exposing (FormInput)
import InputValidation as Validate exposing (TypedInput(..))
import Json.Decode as Json
import Json.Encode
import Dict


--INPUT READERS

{-| Given an `id` string and a `FormInput` dictionary, attempt to look up the
value captured from the element with that `id` and decode it as a string.

Returns an error message if no value was captured at that `id`, if the decoder
fails, or if the value is a type other than StringInput.

-}
readStringAt : String -> FormInput a -> Result String String
readStringAt key formInput =
  case formInput |> Dict.get key of
    Just typedInput ->
      typedInput
        |> Validate.readStringInput

    Nothing ->
      "Submitted form does not contain a value for " ++ key
        |> Err


{-| Given an `id` string and a `FormInput` dictionary, attempt to look up the
value captured from the element with that `id` and decode it as an `Int`.

Returns an error message if no value was captured at that `id`, if the decoder
fails, or if the value is a type other than IntInput.

-}
readIntAt : String -> FormInput a -> Result String Int
readIntAt key formInput =
  case formInput |> Dict.get key of
    Just typedInput ->
      typedInput
        |> Validate.readIntInput

    Nothing ->
      "Submitted form does not contain a value for " ++ key
        |> Err


{-| Given an `id` string and a `FormInput` dictionary, attempt to look up the
value captured from the element with that `id` and decode it as a `Float`.

Returns an error message if no value was captured at that `id`, if the decoder
fails, or if the value is a type other than FloatInput.

-}
readFloatAt : String -> FormInput a -> Result String Float
readFloatAt key formInput =
  case formInput |> Dict.get key of
    Just typedInput ->
      typedInput
        |> Validate.readFloatInput

    Nothing ->
      "Submitted form does not contain a value for " ++ key
        |> Err


{-| Given an `id` string and a `FormInput` dictionary, attempt to look up the
value captured from the element with that `id` and decode it as a `Bool`.

Returns an error message if no value was captured at that `id`, if the decoder
fails, or if the value is a type other than BoolInput.

-}
readBoolAt : String -> FormInput a -> Result String Bool
readBoolAt key formInput =
  case formInput |> Dict.get key of
    Just typedInput ->
      typedInput
        |> Validate.readBoolInput

    Nothing ->
      "Submitted form does not contain a value for " ++ key
        |> Err


{-| Given an `id` string and a `FormInput` dictionary, attempt to look up the
value captured from the element with that `id` and decode it with a custom
decoder.

Returns an error message if no value was captured at that `id`, if the decoder
fails, or if the value is a type other than CustomInput.

-}
readCustomAt : String -> FormInput a -> Result String a
readCustomAt key formInput =
  case formInput |> Dict.get key of
    Just typedInput ->
      typedInput
        |> Validate.readCustomInput

    Nothing ->
      "Submitted form does not contain a value for " ++ key
        |> Err


-- CONVERTING FORM INPUT TO JSON

{-| Given a `FormInput` dictionary, return a JSON object containing every
key-value pair in the dictionary, with values converted to corresponding
JavaScript types.

    --simulated input
    input1 = ( "userName", StringInput (Json.Encode.string "Bob") )
    input2 = ("userAge", IntInput (Json.Encode.string "33") )
    formInput = Dict.fromList [ input1, input2 ]

    formInput
      |> toJson

    --> { userAge = 33, userName = "Bob" } : Json.Decode.Value
-}
toJson : FormInput a -> Json.Value
toJson formInput =
  formInput
    |> Dict.toList
    |> List.map
      ( Tuple.mapSecond (toJsonValue >> Result.withDefault Json.Encode.null) )
    |> Json.Encode.object


{-| Convert a `TypedInput` value to a Json `Value` of the corresponding type

-}
toJsonValue : TypedInput a -> Result String Json.Value
toJsonValue typedInput =
  case typedInput of
    StringInput jsonValue ->
      typedInput
        |> Validate.readStringInput
        |> Result.map Json.Encode.string

    IntInput jsonValue ->
      typedInput
        |> Validate.readIntInput
        |> Result.map Json.Encode.int

    FloatInput jsonValue ->
      typedInput
        |> Validate.readFloatInput
        |> Result.map Json.Encode.float

    BoolInput expression jsonValue ->
      typedInput
        |> Validate.readBoolInput
        |> Result.map Json.Encode.bool

    CustomInput decoder jsonValue ->
      jsonValue
        |> Ok

    Fail ->
      "Something went wrong: Attempting to convert `Fail` to JSON"
        |> Err
