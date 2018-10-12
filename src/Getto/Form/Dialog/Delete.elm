module Getto.Form.Dialog.Delete exposing
  ( Model
  , confirm
  , cancel
  , connect
  , request
  , done
  , form
  , dialog
  , message
  )

import Getto
import Getto.I18n as I18n
import Getto.Rest as Rest
import Getto.Moment as Moment
import Getto.Form as Form
import Getto.View.Fa as Fa

import Http

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

type alias Model a =
  { a
  | api : Getto.Api
  , delete : Maybe (Maybe Rest.State)
  }

type alias Content a msg = Form.Content (Model a) msg

confirm : Model a -> ( Model a, Cmd msg )
confirm model =
  case model.delete of
    Just _ -> model ! []
    Nothing -> { model | delete = Just Nothing } ! []

cancel : Model a -> ( Model a, Cmd msg )
cancel model =
  case model.delete of
    Just (Just (Rest.Connecting)) -> model ! []
    _ -> { model | delete = Nothing } ! []

connect = request

request : (Model a -> Rest.Request data) -> (Rest.RestResult data -> msg) -> Model a -> ( Model a, Cmd msg )
request request msg model =
  case model.delete of
    Nothing -> model ! []
    Just state ->
      if state |> Rest.isConnecting
        then model ! []
        else
          { model | delete = Just <| Just Rest.Connecting } !
            [ model.api
              |> request model
              |> Http.send msg
            ]

done : Rest.RestResult data -> Model a -> ( Model a, Cmd msg )
done result model =
  { model
  | delete =
    case result |> Rest.done of
      Nothing    -> Nothing
      Just state -> Just <| Just state
  } ! []


form : msg -> Content a msg
form msg =
  Form.html (H.form [ E.onSubmit msg ])
    [ Form.html (H.h2 []) [ "DELETE" |> Form.text ]
    , Form.submit [ A.class "is-delete" ] [ "delete.submit" |> I18n.t |> Form.text ]
    ]

dialog : ( msg, msg ) -> List (Content a msg) -> List (Content a msg) -> Content a msg
dialog (delete,cancel) header messages model = model |>
  case model.delete of
    Nothing -> Form.none
    Just state ->
      Form.html (H.section [ A.class "dialog" ])
        [ Form.html (H.section [])
          [ Form.html (H.h3 []) header
          , Form.html (H.p []) messages
          , (case state of
              Just (Rest.Err error) ->
                Form.html (H.p [ A.class ("is-error") ])
                  [ error |> Rest.error |> I18n.rest |> Form.text ]
              _ -> Form.none
            )
          , Form.html (H.footer [ A.class "is-center" ])
            (case state of
              Just Rest.Connecting ->
                [ Form.button [ A.class "is-connecting" ]
                  [ always <| Fa.spinner
                  , "delete.connecting" |> I18n.t |> Form.text
                  ]
                ]

              _ ->
                [ (case state of
                    Just (Rest.Err _) -> deleteError  delete
                    _                 -> deleteButton delete
                  )
                , Form.button [ A.class "is-cancel", E.onClick cancel ]
                  [ "close.submit" |> I18n.t |> Form.text ]
                ]
            )
          ]
        ]

deleteButton : msg -> Content a msg
deleteButton msg =
  Form.button [ A.class "is-delete", E.onClick msg ]
    [ "delete.submit" |> I18n.t |> Form.text ]

deleteError : msg -> Content a msg
deleteError msg =
  Form.button [ A.class "is-delete is-error", E.onClick msg ]
    [ "delete.error" |> I18n.t |> Form.text ]


message : (String -> String) -> List String -> List (Content a msg)
message translate = List.map (translate >> Form.text) >> Form.withBr
