port module Getto.Form.Search exposing
  ( Model
  , fixedMidashi
  , connect
  , request
  , done
  , update
  , form
  , new
  , p
  , editLink
  , editButton
  , deleteButton
  , downloadLink
  , textBox
  , textBoxBetween
  , dateBox
  , dateBoxBetween
  , checkList
  , checkListBlock
  , boolList
  , boolListInvert
  , select
  , options
  , data
  , loading
  )

import Getto
import Getto.I18n as I18n
import Getto.Rest as Rest
import Getto.Rest.Search as Search
import Getto.Form as Form
import Getto.Form.Search.Field as Field
import Getto.Form.Search.Pager as Pager
import Getto.Form.Search.Data as Data
import Getto.View.Fa as Fa

import Http

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

port fixedMidashi : () -> Cmd msg

type alias Model data row a = Field.Model
  { a
  | api : Getto.Api
  , search : Maybe Rest.State
  , response : Maybe (Response data row)
  }

type alias Request      data row = Rest.Request (Response data row)
type alias SearchResult data row = Rest.RestResult (Response data row)
type alias Response     data row = Search.Response data row

type alias Pairs model msg = List ( List (Form.Content model msg), List (Form.Content model msg) )

type alias Modify msg = Field.Method -> Field.Modify -> msg


connect = request

request : (( Model data row a, Rest.JsonBody ) -> Request data response) -> (Model data row a -> Cmd msg) -> (SearchResult data response -> msg) -> Model data row a -> ( Model data row a, Cmd msg )
request request save msg model =
  if model.search |> Rest.isConnecting
    then model ! []
    else
      { model | search = Just Rest.Connecting } !
        [ model.api
          |> request
            ( model
            , model |> Field.encodeFields
            )
          |> Http.send msg
        , model |> save
        ]

done : SearchResult data response -> (response -> row) -> Model data row a -> ( Model data row a, Cmd msg )
done result init model =
  { model
  | search = result |> Rest.done
  , response =
    case result of
      Ok response -> Just { response | rows = response.rows |> List.map init }
      Err _       -> model.response
  } ! [ if result |> Result.toMaybe |> (/=) Nothing then fixedMidashi () else Cmd.none ]

update : (row -> key) -> key -> (row -> ( row, Cmd msg )) -> Model data row a -> ( Model data row a, Cmd msg )
update prop key f model =
  case model.response of
    Nothing -> model ! []
    Just response ->
      let
        (rows,cmds) = response.rows |> List.foldl
          (\row (rows,cmds) ->
            if (row |> prop) /= key
              then ( rows ++ [row], cmds )
              else
                let
                  (newRow,cmd) = row |> f
                in
                  ( rows ++ [newRow], cmds ++ [cmd] )
          )
          ([],[])
      in
        { model | response = Just { response | rows = rows } } ! cmds


form : msg -> List (Pairs (Model data row a) msg) -> List (Form.Content (Model data row a) msg) -> Form.Content (Model data row a) msg
form msg pairs buttons =
  Form.html (H.section [ A.class "search" ])
    [ Form.html (H.form [ E.onSubmit msg ])
      [ Form.html (H.section []) (pairs |> List.map table)
      , Form.html (H.footer  []) (search :: error :: buttons)
      ]
    ]

table : Pairs model msg -> Form.Content model msg
table =
  List.map
    (\(header,column) ->
      Form.html (H.tr [])
        [ Form.html (H.th []) header
        , Form.html (H.td []) column
        ]
    )
  >> Form.html (H.table [])

search : Form.Content (Model data row a) msg
search model = model |>
  case model.search of
    Just Rest.Connecting ->
      Form.button [ A.class "is-connecting" ]
        [ always <| Fa.spinner
        , "search.connecting" |> I18n.t |> Form.text
        ]
    Just (Rest.Err _) ->
      Form.submit [ A.class "is-error" ]
        [ "search.error" |> I18n.t |> Form.text ]
    Nothing ->
      Form.submit []
        [ "search.submit" |> I18n.t |> Form.text ]

error : Form.Content (Model data row a) msg
error model = model |>
  case model.search of
    Just (Rest.Err error) ->
      error |> Rest.error |> I18n.rest |> Form.badge "is-danger is-small"
    _ -> Form.none


new : String -> Form.Content model msg
new href = always <|
  H.a [ A.href href ]
    [ Fa.solid "plus" []
    , H.text " "
    , "link.create" |> I18n.t |> H.text
    ]

p : String -> Html msg
p content = H.p [] [ content |> H.text ]

editLink : String -> Html msg
editLink href =
  H.p []
    [ H.a [ A.href href ]
      [ Fa.solid "edit" []
      , H.text " "
      , "link.edit" |> I18n.t |> H.text
      ]
    ]

editButton : (model -> msg) -> Form.Content model msg
editButton msg model =
  H.button [ A.class "is-edit", E.onClick (msg model) ] [ Fa.solid "pencil-alt" [] ]

deleteButton : (model -> msg) -> Form.Content model msg
deleteButton msg model =
  H.button [ A.class "is-delete", E.onClick (msg model) ] [ Fa.solid "times" [] ]

downloadLink : String -> Html msg
downloadLink href =
  H.p []
    [ H.a [ A.href <| href ]
      [ Fa.solid "file" []
      , H.text " "
      , "link.download" |> I18n.t |> H.text
      ]
    ]


textBox : Modify msg -> Field.Method -> Form.Content (Model data row a) msg
textBox = textInput "text"

textBoxBetween : Modify msg -> String -> Form.Content (Model data row a) msg
textBoxBetween msg key =
  Form.html (H.div [])
    [ key |> Field.GTEQ |> textBox msg
    , " - " |> Form.text
    , key |> Field.LTEQ |> textBox msg
    ]

dateBox : Modify msg -> Field.Method -> Form.Content (Model data row a) msg
dateBox = textInput "date"

dateBoxBetween : Modify msg -> String -> Form.Content (Model data row a) msg
dateBoxBetween msg key =
  Form.html (H.div [])
    [ key |> Field.GTEQ_DATETIME |> dateBox msg
    , " - " |> Form.text
    , key |> Field.LTEQ_DATETIME |> dateBox msg
    ]

textInput : String -> Modify msg -> Field.Method -> Form.Content (Model data row a) msg
textInput type_ msg method model =
  H.input
    [ A.type_ type_
    , A.value (model |> Field.get method)
    , E.onInput (Field.Set >> msg method)
    ] []

checkList : Modify msg -> (String -> String) -> Field.Method -> Form.Content (Model data row a) msg
checkList = checkListWithAttr []

checkListBlock : Modify msg -> (String -> String) -> Field.Method -> Form.Content (Model data row a) msg
checkListBlock = checkListWithAttr [ A.class "is-block" ]

checkListWithAttr : List (H.Attribute msg) -> Modify msg -> (String -> String) -> Field.Method -> Form.Content (Model data row a) msg
checkListWithAttr attrs msg i18n method model =
  let
    values = model |> Field.values method
  in
    model
    |> Field.options method
    |> options i18n
    |> List.map
      (\(option,label) -> H.li []
        [ H.label []
          [ H.input
            [ A.type_ "checkbox"
            , A.checked (values |> List.member option)
            , E.onClick (Field.Check option |> msg method)
            ] []
          , " "   |> H.text
          , label |> H.text
          ]
        ]
      )
    |> H.ul attrs

boolList : Modify msg -> (Bool -> String) -> Field.Method -> Form.Content (Model data row a) msg
boolList = boolListBox [ True, False ]

boolListInvert : Modify msg -> (Bool -> String) -> Field.Method -> Form.Content (Model data row a) msg
boolListInvert = boolListBox [ False, True ]

boolListBox : List Bool -> Modify msg -> (Bool -> String) -> Field.Method -> Form.Content (Model data row a) msg
boolListBox values msg i18n method model =
  let
    bools = model |> Field.bools method
  in
    values
    |> options i18n
    |> List.map
      (\(option,label) -> H.li []
        [ H.label []
          [ H.input
            [ A.type_ "checkbox"
            , A.checked (bools |> List.member option)
            , E.onClick (Field.CheckBool option |> msg method)
            ] []
          , " "   |> H.text
          , label |> H.text
          ]
        ]
      )
    |> H.ul []

select : Modify msg -> List ( String, String ) -> Field.Method -> Form.Content (Model data row a) msg
select msg values method model =
  let
    value = model |> Field.get method
  in
    values
    |> List.append [("", "select.all" |> I18n.t)]
    |> List.map
      (\(option,label) ->
        H.option
          [ A.value option
          , A.selected (option == value)
          ]
          [ H.text label ]
      )
    |> H.select [ E.onInput (Field.Set >> msg method) ]

options : (value -> String) -> List value -> List ( value, String )
options toLabel = List.map (\value -> (value, value |> toLabel))


data : ( Int -> msg, String -> msg ) -> List (Data.Cell (Response data row) row msg) -> Form.Content (Model data row a) msg
data (page,sort) cells model =
  case model.response of
    Nothing -> H.text ""
    Just response ->
      let
        paging = (model.fields.page, response.max) |> Pager.paging page
        data   = (model.fields.sort, (response, response.rows) ) |> Data.table sort cells
      in
        H.section [ A.class "data" ] <|
          if response.max == 0
            then
              [ paging
              , data
              ]
            else
              [ paging
              , data
              , paging
              ]

loading : Form.Content (Model data row a) msg -> Form.Content (Model data row a) msg
loading contents model =
  case model.search of
    Just Rest.Connecting ->
      H.section [ A.class "loading" ]
        [ H.section [] [ Fa.spinner ] ]
    Just (Rest.Err error) ->
      H.section [ A.class "loading" ]
        [ H.section [] [ error |> Rest.error |> I18n.rest |> H.text ] ]
    Nothing ->
      model |> contents
