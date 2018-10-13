module Getto.Rest.Path exposing
  ( withID
  )

import Http

withID : Int -> String -> String
withID id = replace "id" (id |> toString)

replace : String -> String -> String -> String
replace key value =
  let
    target = ":" ++ key
    replacement = value |> encode target
  in
    String.split target >> String.join replacement

encode target value =
  case value of
    "" -> target
    val -> val |> Http.encodeUri
