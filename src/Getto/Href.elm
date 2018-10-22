module Getto.Href exposing
  ( href
  , url
  , prepend
  , chop
  )

import Getto.Env as Env
import Getto.Location as Location

import Http

href : Location.Search -> String -> String
href params = prepend Env.pageRoot >> withSearch params

url : Location.Search -> String -> String
url params = prepend Env.apiHost >> withSearch params

prepend : String -> String -> String
prepend prefix href = href |>
  if href |> String.startsWith prefix
    then identity
    else String.append prefix

withSearch : Location.Search -> String -> String
withSearch params href =
  case params |> encode of
    "" -> href
    searchString ->
      if href |> String.contains "?"
        then href ++ "&" ++ searchString
        else href ++ "?" ++ searchString

encode : List ( String, String ) -> String
encode =
  List.map
    (\(key,value) ->
      (key |> Http.encodeUri) ++ "=" ++ (value |> Http.encodeUri)
    )
  >> String.join "&"


chop : String -> String
chop = chopPageRoot >> chopSearch

chopPageRoot : String -> String
chopPageRoot href = href |>
  if href |> String.startsWith Env.pageRoot
    then String.dropLeft (Env.pageRoot |> String.length)
    else identity

chopSearch : String -> String
chopSearch = String.split "?" >> List.head >> Maybe.withDefault ""
