import InputValidation exposing (TypedInput(..))
import FormCapture as Capture exposing (FormInput)
import FormCapture.Readers as Readers
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Json
import List


main =
  Html.beginnerProgram
    { model = userInput
    , view = view
    , update = update
    }


-- IDs

id =
  { name = "name"
  , age = "age"
  , pi = "pi"
  , answer = "answer"
  , chars = "chars"
  }


-- MODEL

type alias UserInput =
  { name : Maybe String
  , age : Maybe Int
  , pi : Maybe Float
  , answer : Maybe Bool
  , chars : Maybe (List Char)
  }

userInput =
  { name = Nothing
  , age = Nothing
  , pi = Nothing
  , answer = Nothing
  , chars = Nothing
  }


-- UPDATE

type Msg a
  = Submit (FormInput a)


update : Msg (List Char) -> UserInput -> UserInput
update msg userInput =
  case msg of
    Submit formSubmit ->
      { userInput
      | name =
          formSubmit
            |> Readers.readStringAt id.name
            |> Result.toMaybe
      , age =
          formSubmit
            |> Readers.readIntAt id.age
            |> Result.toMaybe

      , pi =
          formSubmit
           |> Readers.readFloatAt id.pi
           |> Result.toMaybe

      , answer =
          formSubmit
            |> Readers.readBoolAt id.answer
            |> Result.toMaybe

      , chars =
          formSubmit
            |> Readers.readCustomAt id.chars
            |> Result.toMaybe
      }


-- VIEW

view : UserInput -> Html (Msg (List Char))
view userInput =
  let
    formElement (idString, inputKey, labelText) =
      [ Html.label [] [ Html.text labelText ]
      , Html.br [] []
      , Html.input [ Attr.type_ "text", Attr.id idString ] []
      ]
        |> Html.div []
        |> Capture.elementFromHtml (idString, inputKey)

    form =
      [ ( id.name
        , StringInput
        , "What is your name?"
        )
      , ( id.age
        , IntInput
        , "How old are you?"
        )
      , ( id.pi
        , FloatInput
        , "Enter the first few digits of Pi:"
        )
      , ( id.answer
        , BoolInput (\s -> String.toLower s == "platypus")
        , "Type the word PLATYPUS:"
        )
      , ( id.chars
        , CustomInput (Json.string |> Json.map String.toList)
        , "Type some characters!"
        )
      ]
        |> List.map formElement
        |> Capture.newForm Submit
        |> Capture.formToHtml
        |> (\n -> [n])
        |> Html.div [ Attr.style divStyles ]

    output =
      [ userInput.name
        |> Maybe.map (\s -> "\"" ++ s ++ "\"")
        |> Maybe.withDefault "Expecting a `String`"
      , userInput.age
        |> Maybe.map toString
        |> Maybe.withDefault "Expecting an `Int`"
      , userInput.pi
        |> Maybe.map toString
        |> Maybe.withDefault "Expecting a `Float`"
      , userInput.answer
        |> Maybe.map (\b -> if b then "Correct" else "Incorrect")
        |> Maybe.withDefault "Expecting a `Bool` result"
      , userInput.chars
        |> Maybe.map (\list -> list |> List.intersperse ',' |> String.fromList)
        |> Maybe.map (\s -> "[" ++ s ++ "]")
        |> Maybe.withDefault "Expecting a `Char` sequence"
      ]
        |> List.map (\n -> Html.div [] [Html.br [] [], Html.text n])
        |> Html.div [ Attr.style (("color", "red") :: divStyles) ]

    divStyles =
      [ ("display", "inline-block")
      , ("vertical-align", "top")
      , ("padding", "0 10px")
      , ("line-height", "1.5em")
      ]

  in
    [ form
    , output
    ]
      |> Html.div []
