module Getto.Form.Dialog.Download exposing
  ( Model
  , FetchResult
  , confirm
  , cancel
  , connect
  , request
  , done
  , dialog
  , message
  )

import Getto
import Getto.I18n as I18n
import Getto.Rest as Rest
import Getto.Auth as Auth
import Getto.Location as Location
import Getto.Form as Form
import Getto.View.Fa as Fa

import Http
import Dict exposing ( Dict )

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

type alias Model a =
  { a
  | api : Getto.Api
  , download : Maybe (Dict String Rest.State)
  }

type alias FetchResult = Rest.RestResult Auth.Token

type alias Content a msg = Form.Content (Model a) msg

confirm : Model a -> ( Model a, Cmd msg )
confirm model =
  case model.download of
    Just _ -> model ! []
    Nothing -> { model | download = Just Dict.empty } ! []

cancel : Model a -> ( Model a, Cmd msg )
cancel model =
  case model.download of
    Nothing -> model ! []
    Just dict ->
      if dict |> Dict.toList |> List.any (\(_,state) -> state == Rest.Connecting)
        then model ! []
        else { model | download = Nothing } ! []

connect = request

request : Rest.Request Auth.Token -> String -> (String -> FetchResult -> msg) -> Model a -> ( Model a, Cmd msg )
request request file msg model =
  case model.download of
    Nothing -> model ! []
    Just dict ->
      if dict |> Dict.get file |> Rest.isConnecting
        then model ! []
        else
          { model | download = Just (dict |> Dict.update file (always <| Just Rest.Connecting)) } !
            [ model.api
              |> request
              |> Http.send (msg file)
            ]

done : FetchResult -> String -> (String -> Model a -> Getto.Api -> String) -> Model a -> ( Model a, Cmd msg )
done result file download model =
  case model.download of
    Nothing -> model ! []
    Just dict ->
      { model | download = Just (dict |> Dict.update file (always <| Rest.done result)) } !
        [ result
          |> Result.toMaybe
          |> Maybe.map
            (\auth ->
              { token = Just auth.token } |> download file model |> Location.redirectTo
            )
          |> Maybe.withDefault Cmd.none
        ]


dialog : ( String -> msg, msg ) -> List String -> (String -> String) -> List (Content a msg) -> List (Content a msg) -> Content a msg
dialog (download,cancel) files i18n header messages model = model |>
  case model.download of
    Nothing -> Form.none
    Just dict ->
      let
        errors =
          files
          |> List.filterMap
            (\file ->
              case dict |> Dict.get file of
                Just (Rest.Err error) -> Just (error |> Rest.error |> I18n.rest |> Form.text)
                _ -> Nothing
            )

        buttons =
          files
          |> List.map
            (\file ->
              case dict |> Dict.get file of
                Just Rest.Connecting ->
                  Form.button [ A.class "is-connecting" ]
                    [ always <| Fa.spinner
                    , "download.connecting" |> I18n.t |> Form.text
                    ]
                Just (Rest.Err _) -> downloadError  download file i18n
                _                 -> downloadButton download file i18n
            )
      in
        Form.html (H.section [ A.class "dialog" ])
          [ Form.html (H.section [])
            [ Form.html (H.h3 []) header
            , Form.html (H.p []) messages
            , (if errors |> List.isEmpty
                then Form.none
                else Form.html (H.p [ A.class ("is-error") ]) errors
              )
            , Form.html (H.footer [ A.class "is-center" ])
              (List.append buttons
                [ Form.button [ A.class "is-cancel", E.onClick cancel ]
                  [ "close.submit" |> I18n.t |> Form.text ]
                ]
              )
            ]
          ]

downloadButton : (String -> msg) -> String -> (String -> String) -> Content a msg
downloadButton msg file i18n =
  Form.button [ E.onClick (msg file) ]
    [ file |> i18n |> Form.text ]

downloadError : (String -> msg) -> String -> (String -> String) -> Content a msg
downloadError msg file i18n =
  Form.button [ A.class "is-error", E.onClick (msg file) ]
    [ file |> i18n |> Form.text ]


message : (String -> String) -> List String -> List (Content a msg)
message translate = List.map (translate >> Form.text) >> Form.withBr
