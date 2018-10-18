module Getto.Rest exposing
  ( State(..)
  , Headers
  , JsonBody
  , MultipartBody
  , Request
  , RestResult
  , isConnecting
  , get
  , create
  , update
  , delete
  , upload
  , done
  , without
  , error
  , withID
  , withRelatedID
  , withIDSearch
  , key
  )

import Getto.Model.Api exposing ( Api )

import Getto.Env as Env
import Getto.Location as Location
import Getto.Href as Href
import Getto.Rest.Part as Part

import Http
import Json.Decode as Decode
import Json.Encode as Encode

type State
  = Connecting
  | Err Http.Error

type alias Headers = Api -> List (Http.Header)

type alias JsonBody = List ( String, Encode.Value )
type alias MultipartBody = List ( String, Part.Part )

type alias Request a = Api -> Http.Request a
type alias RestResult a = Result Http.Error a

isConnecting : Maybe State -> Bool
isConnecting = (==) (Just Connecting)

get : Headers -> Decode.Decoder a -> List ( String, Location.Search ) -> String -> Api -> Http.Request a
get headers decoder data path api =
  request (always Http.emptyBody) "GET" headers decoder () (path |> Href.url (data |> search)) api

create = request jsonBody "POST"
update = request jsonBody "PUT"
delete = request jsonBody "DELETE"

upload = request multipartBody "POST"

request : (data -> Http.Body) -> String -> Headers -> Decode.Decoder a -> data -> String -> Api -> Http.Request a
request toBody method headers decoder data path api = Http.request <|
  { method = method
  , headers = api |> headers
  , url = url path
  , body = data |> toBody
  , expect = Http.expectJson decoder
  , timeout = Nothing -- TODO タイムアウトを設定するべき
  , withCredentials = False
  }

done : RestResult a -> Maybe State
done result =
  case result of
    Result.Ok _ -> Nothing
    Result.Err error -> Just <| Err error

url : String -> String
url path = Env.apiHost ++ path

jsonBody : List ( String, JsonBody ) -> Http.Body
jsonBody =
  List.map (\(name, list) -> ( name, list |> Encode.object ))
  >> Encode.object
  >> Http.jsonBody

multipartBody : List ( String, MultipartBody ) -> Http.Body
multipartBody =
  List.foldr
    (\(group, list) acc ->
      list
      |> List.foldr
        (\(name, part) acc ->
          acc ++ ((key [group,name], part) |> Part.toParts)
        )
        acc
    )
    []
  >> Http.multipartBody

search : List ( String, Location.Search ) -> Location.Search
search =
  List.foldr
    (\(group, list) acc ->
      list
      |> List.foldr
        (\(name, value) acc ->
          acc ++ [(key [group,name], value)]
        )
        acc
    )
    []

without : List String -> List ( String, a ) -> List ( String, a )
without names = List.filter <|
  \(name,_) -> not <| List.member name names

error : Http.Error -> String
error error =
  case error of
    Http.BadUrl _       -> "bad-url"
    Http.Timeout        -> "timeout"
    Http.NetworkError   -> "network-error"
    Http.BadPayload e _ -> e |> Debug.log "bad-payload" |> always "bad-payload"
    Http.BadStatus response ->
      case response.status.code of
        401 -> "unauthorized"
        404 -> "not-found"
        405 -> "method-not-allowed"
        409 -> "conflict"
        422 -> "unprocessable-entity"
        _   -> "bad-status"


withID : Int -> JsonBody -> JsonBody
withID = withRelatedID "id"

withRelatedID : String -> Int -> JsonBody -> JsonBody
withRelatedID name id = List.append [ (name, id |> Encode.int) ]

withIDSearch : Int -> Location.Search -> Location.Search
withIDSearch id = List.append [ ("id", id |> toString) ]

key : List String -> String
key keys =
  case keys of
    [] -> ""
    head :: tail -> head ++ (tail |> List.map (\key -> "[" ++ key ++ "]") |> String.join "")
