module Getto.Form.Fetch exposing
  ( Model
  , connect
  , request
  , done
  , form
  )

import Getto.Model.Api exposing ( Api )
import Getto.I18n as I18n
import Getto.Rest as Rest
import Getto.View.Fa as Fa

import Http

import Html as H exposing ( Html )
import Html.Attributes as A

type alias Model a =
  { a
  | api : Api
  , fetch : Maybe Rest.State
  }

connect = request

request : (Model a -> Rest.Request data) -> (Rest.RestResult data -> msg) -> Model a -> ( Model a, Cmd msg )
request request msg model =
  if model.fetch |> Rest.isConnecting
    then model ! []
    else
      { model | fetch = Just Rest.Connecting } !
        [ model.api
          |> request model
          |> Http.send msg
        ]

done : Rest.RestResult data -> Model a -> ( Model a, Cmd msg )
done result model =
  { model | fetch = result |> Rest.done } ! []

form : List (Html msg) -> Model a -> List (Html msg)
form contents model =
  case model.fetch of
    Just Rest.Connecting ->
      [ H.section [ A.class "loading" ]
        [ H.section [] [ Fa.spinner ] ]
      ]
    Just (Rest.Err error) ->
      [ H.section [ A.class "loading" ]
        [ H.section [] [ error |> Rest.error |> I18n.rest |> H.text ] ]
      ]
    Nothing ->
      contents
