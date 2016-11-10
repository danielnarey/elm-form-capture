module FormCapture exposing
  ( FormInput, Form, FormElement, elementFromHtml, elementFromHtmlTree, newForm
  , addFormElement, formToHtml, formToHtmlTree
  )

{-|

## Capture form input as a dictionary keyed by component id

The Elm `Html` package lacks a set of built-in functions for capturing input
from a form with multiple fields, and implementing form capture from scratch
can be tricky. The `FormCapture` library makes it easy to capture form input by
abstracting out a lot of the implementation details. You only need to specify an
`id` string and input type key for each form element, and the rendering function
uses that data to generate a decoder that will be applied on a `submit` event.
The HTML specification for each form element is entirely customizable using
standard `Html` or the alternative
[`HtmlTree`](http://package.elm-lang.org/packages/danielnarey/elm-html-tree/)
syntax.

# Input Representation
@docs FormInput

# Form Representation
@docs Form, FormElement

# Constructing Form Elements
@docs elementFromHtml, elementFromHtmlTree

# Constructing a Form
@docs newForm, addFormElement

# Rendering a Form
@docs formToHtml, formToHtmlTree

See
[examples/FormInput.elm](https://github.com/danielnarey/elm-form-capture/blob/master/examples/)
for a full working example.
-}

import Toolkit.Operators exposing (..)
import Toolkit.Helpers as Helpers
import InputValidation exposing (TypedInput(..))
import HtmlTree exposing (HtmlTree)
import Html exposing (Html)
import Html.Events as Events
import Json.Decode as Json exposing (Decoder)
import Dict exposing (Dict)
import String


-- INPUT REPRESENTATION

{-| Represents input captured from a form with multiple fields. Implemented as a
dictionary, where the *key* is the `id` of the input element and the *value* is
its captured value as
[`TypedInput`](http://package.elm-lang.org/packages/danielnarey/elm-input-validation/latest/InputValidation#TypedInput).
-}
type alias FormInput a =
  Dict String (TypedInput a)


-- FORM REPRESENTATION

{-| Represents an HTML form
-}
type alias Form msg a =
  { captureKey : (FormInput a -> msg)
  , elements : List (FormElement msg a)
  }


{-| Represents an HTML form element
-}
type alias FormElement msg a =
  { id : String
  , inputKey : (Json.Value -> TypedInput a)
  , component : FormComponent msg
  }


{-| Allows the HTML specification of a `FormElement` to use the standard `Html`
syntax or the alternative
[`HtmlTree`](http://package.elm-lang.org/packages/danielnarey/elm-html-tree/)
syntax.
-}
type FormComponent msg
  = Standard (Html msg)
  | Tree (HtmlTree msg)


-- CONSTRUCTING FORM ELEMENTS

{-| Create a form element by supplying a tuple containing an id string and a
[`TypedInput`](http://package.elm-lang.org/packages/danielnarey/elm-input-validation/latest/InputValidation#TypedInput)
key as the first arugment and an `Html` node containing the input field or
selector from which input will be captured as the second argument. For form
capture to succeed, the input or select element contained within the node
must be assigned an `id` attribute matching the one passed to this function.

Use this function if you are generating your view components with the standard
`Html` package.

    import Html
    import Html.Attributes as Attr
    ...

    [ Html.label [] [ "What is your name?" ]
    , Html.input [ Attr.type' "text", Attr.id "name" ] []
    ]
      |> Html.div []
      |> elementFromHtml ("name", StringInput)

-}
elementFromHtml : (String, Json.Value -> TypedInput a) -> Html msg -> FormElement msg a
elementFromHtml (idString, inputKey) node =
  { id = idString
  , inputKey = inputKey
  , component = Standard node
  }


{-| Create a form element by supplying a tuple containing an id string and a
[`TypedInput`](http://package.elm-lang.org/packages/danielnarey/elm-input-validation/latest/InputValidation#TypedInput)
key as the first arugment and an
[`HtmlTree`](http://package.elm-lang.org/packages/danielnarey/elm-html-tree/latest/HtmlTree)
containing the input field or selector from which input will be captured as the
second argument. For form capture to succeed, the input or select element
contained within the node must be assigned an `id` attribute matching the one
passed to this function.

Use this function if you you are generating your view components with the
non-standard
[`HtmlTree`](http://package.elm-lang.org/packages/danielnarey/elm-html-tree/)
package.

    import HtmlTree as Tree
    ...

    [ "What is your name?"
      |> Tree.textWrapper "label"
    , Tree.leaf "input"
      |> Tree.withId "name"
      |> Tree.addAttribute ("type", "text")
    ]
      |> Tree.container "div"
      |> elementFromHtmlTree ("name", StringInput)

-}
elementFromHtmlTree : (String, Json.Value -> TypedInput a) -> HtmlTree msg -> FormElement msg a
elementFromHtmlTree (idString, inputKey) tree =
  { id = idString
  , inputKey = inputKey
  , component = Tree tree
  }


-- CONSTRUCTING A FORM

{-| Create a `Form` object by supplying the type key of a message that accepts
`FormInput` and a list of `FormElement` records.

    type Msg a
      = Submit (FormInput a)
    ...

    let
      formElements =
        [
          ...
        ]

    in
      formElements
        |> newForm Submit

-}
newForm : (FormInput a -> msg) -> List (FormElement msg a) -> Form msg a
newForm captureKey elementList =
  { captureKey = captureKey
  , elements = elementList
  }


{-| Add a new element to a form, *retaining* any existing elements

    let
      myForm =
        [
          ...
        ]
          |> newForm Submit

      newElement =
        [
          ...
        ]
          |> elementFromHtml ("myId", StringInput)

    in
      myForm
        |> addElement newElement

-}
addFormElement : FormElement msg a -> Form msg a -> Form msg a
addFormElement newElement form =
  { form
  | elements =
      form.elements
        |:: newElement
  }


-- RENDERING A FORM

{-| Render a form to standard `Html`. Use this function if you are generating
your view components with the standard `Html` package.

    div [] [ myForm |> toHtml ]

A submit button for the form will be added as the last form element when it is
rendered to `Html`. The button is assigned class "submit-button" to make it
easier to style with CSS.
-}
formToHtml : Form msg a -> Html msg
formToHtml form =
  form
    |> formToHtmlTree
    |> HtmlTree.assembleHtml


{-| Convert a form to an
[`HtmlTree`](http://package.elm-lang.org/packages/danielnarey/elm-html-tree/latest/HtmlTree).
Use this function if you you are generating your view components with the
non-standard
[`HtmlTree`](http://package.elm-lang.org/packages/danielnarey/elm-html-tree/)
package.

    myForm
      |> toHtmlTree
      |> (\n -> [n])
      |> container "div"

A submit button for the form will be added as the last form element when it is
converted to an `HtmlTree`. The button is assigned class "submit-button" to
make it easier to style with CSS.
-}
formToHtmlTree : Form msg a -> HtmlTree msg
formToHtmlTree form =
  let
    elementToTree someElement =
      case someElement.component of
        Tree tree ->
          tree

        Standard node ->
          node
            |> HtmlTree.opaque

    submitButton =
      HtmlTree.leaf "input"
        |> HtmlTree.addAttribute ("type", "submit")
        |> HtmlTree.addClass ("submit-button")

    captureFormInput someForm =
      someForm.elements
        |> generateFormDecoder
        |> Json.map someForm.captureKey
        |> captureOnSubmit

    captureOnSubmit =
      { stopPropagation = False
      , preventDefault = True
      }
        |> Events.onWithOptions "submit"

  in
    form.elements
      .|> elementToTree
      |:: submitButton
      |> HtmlTree.container "form"
      |> HtmlTree.withObserver (form ||> captureFormInput)


{-| Given a list of `FormElement` records, this function generates a decoder
that will capture form input as a `Dict`, where the *key* is the `id` of the
input element and the *value* is `TypedInput`.

-}
generateFormDecoder : List (FormElement msg a) -> Decoder (FormInput a)
generateFormDecoder elementList =
  let
    generateDecoderList elementList decoderList =
      case elementList ||> List.head of
        Just nextElement ->
          nextElement
            |> constructElementDecoder
            |> Json.object2 (|::) decoderList
            |> generateDecoderList (elementList ||> List.drop 1)

        Nothing ->
          decoderList

    constructElementDecoder formElement =
      Json.value
        |> Json.at ["target", "elements", formElement.id, "value"]
        |> Json.map (\v -> (formElement.id, formElement.inputKey v) )

  in
    Json.succeed []
      |> generateDecoderList elementList
      |> Json.map Dict.fromList
