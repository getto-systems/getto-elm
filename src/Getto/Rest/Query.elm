module Getto.Rest.Query exposing
  ( Search
  , SearchValue
  , string
  , list
  , group
  , search
  )

import Getto.Location as Location

type SearchValue
  = SearchString String
  | SearchList (List String)
  | SearchGroup ( List ( String, SearchValue ) )

type alias Search = List ( String, SearchValue )

string : String -> SearchValue
string = SearchString

list : List String -> SearchValue
list = SearchList

group : List ( String, SearchValue ) -> SearchValue
group = SearchGroup

search : Search -> Location.Search
search = List.concatMap <|
  \(key, value) ->
    case value of
      SearchString val -> [ ( key, val ) ]
      SearchList vals ->
        if vals |> List.isEmpty
          then [ ( key ++ "[]", "" ) ]
          else vals |> List.map (\val -> ( key ++ "[]", val ))
      SearchGroup group ->
        group
        |> List.map
          (\(subKey, val) ->
            ( case subKey |> String.split "[" of
              [] -> key
              head :: tail ->
                [key] ++ ((head ++ "]") :: tail) |> String.join "["
            , val
            )
          )
        |> search
