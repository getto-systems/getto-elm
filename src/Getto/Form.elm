module Getto.Form exposing
  ( Content
  , text
  , none
  , maybe
  , br
  , badge
  , submit
  , button
  , html
  , withSpace
  , withBr
  )

import Html as H exposing ( Html )
import Html.Attributes as A

type alias Content model msg = model -> Html msg

text : String -> Content model msg
text message = always <| H.text message

none : Content model msg
none = text ""

maybe : Content model msg -> Maybe model -> Html msg
maybe f = Maybe.map f >> Maybe.withDefault (H.text "")

br : Content model msg
br = always <| H.br [] []

badge : String -> String -> Content model msg
badge class message =
  html (H.em [ A.class <| "badge " ++ class ])
    [ message |> text ]

submit : List (H.Attribute msg) -> List (Content a msg) -> Content a msg
submit attrs = html (H.button attrs)

button : List (H.Attribute msg) -> List (Content a msg) -> Content a msg
button attrs = html (H.button <| (A.type_ "button") :: attrs)

html : (List (Html msg) -> Html msg) -> List (Content model msg) -> Content model msg
html node contents model =
  contents
  |> withSpace
  |> List.map (\content -> model |> content)
  |> node

withSpace : List (Content model msg) -> List (Content model msg)
withSpace = List.intersperse (text " ")

withBr : List (Content model msg) -> List (Content model msg)
withBr = List.intersperse br
