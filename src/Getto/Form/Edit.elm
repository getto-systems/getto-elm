module Getto.Form.Edit exposing
  ( Model
  , Content
  , Cell(..)
  , Modify
  , connect
  , upload
  , request
  , done
  , continue
  , registered
  , form
  , editForm
  , header
  , table
  , validationState
  , validationClass
  , validationMessages
  , help
  , footer
  , swap
  , edit
  , editSub
  , cancelSub
  , save
  , create
  , commit
  , commitWith
  , error
  , errorWith
  , cancel
  , textBox
  , textBoxLarge
  , textBoxXLarge
  , telBox
  , numberBox
  , numberBoxLarge
  , zipcodeBox
  , regionBox
  , localityBox
  , addressBox
  , dateBox
  , emailBox
  , passwordBox
  , fileBox
  , textarea
  , toggleBox
  , boolList
  , boolListInline
  , radioList
  , radioListInline
  , checkList
  , checkListInline
  , select
  , options
  )

import Getto.Model.Api exposing ( Api )

import Getto.I18n as I18n
import Getto.Rest as Rest
import Getto.Moment as Moment
import Getto.Location as Location
import Getto.Form as Form
import Getto.Form.Edit.Field as Field
import Getto.Form.Edit.Field.Validate as Validate
import Getto.View.Fa as Fa

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

import Http
import Dict

import FileReader

type alias Model a = Field.Model
  { a
  | api : Api
  , update : Maybe Rest.State
  }

type alias Content a msg = Form.Content (Model a) msg
type alias Swap    a msg = ( List (Content a msg), List (Content a msg) )

type Cell a msg
  = Cell               (List String) (List (Content a msg)) (Swap a msg)
  | EditCell           (List String) (List (Content a msg)) (List (Content a msg))
  | StaticCell                       (List (Content a msg)) (List (Content a msg))
  | SubCell     String (List String) (List (Content a msg)) ( List (Content a msg), Swap a msg )
  | SubEditCell String (List String) (List (Content a msg)) (Swap a msg)

type alias Modify msg = String -> Field.Modify -> msg


connect : (( Model a, Rest.JsonBody ) -> Rest.Request data) -> (Rest.RestResult data -> msg) -> Model a -> ( Model a, Cmd msg )
connect = connectWith Field.encodeFields

upload : (( Model a, Rest.MultipartBody ) -> Rest.Request data) -> (Rest.RestResult data -> msg) -> Model a -> ( Model a, Cmd msg )
upload = connectWith Field.multipartFields

connectWith : (Model a -> fields) -> (( Model a, fields ) -> Rest.Request data) -> (Rest.RestResult data -> msg) -> Model a -> ( Model a, Cmd msg )
connectWith toFields request msg model =
  if (model |> Field.hasError) || (model.update |> Rest.isConnecting)
    then model ! []
    else
      { model | update = Just Rest.Connecting } !
        [ model.api
          |> request
            ( model
            , model |> toFields
            )
          |> Http.send msg
        ]

request : (Model a -> Rest.Request data) -> (Rest.RestResult data -> msg) -> Model a -> ( Model a, Cmd msg )
request request msg model =
  if (model |> Field.hasError) || (model.update |> Rest.isConnecting)
    then model ! []
    else
      { model | update = Just Rest.Connecting } !
        [ model.api
          |> request model
          |> Http.send msg
        ]

done : Rest.RestResult data -> Model a -> ( Model a, Cmd msg )
done result model =
  { model | update = result |> Rest.done }
  |>
    (case result of
      Ok  _ -> Field.static
      Err _ -> identity
    )
  |> Moment.nop

continue : Rest.RestResult data -> Model a -> ( Model a, Cmd msg )
continue result model =
  { model | update = result |> Rest.done } ! []

registered : Rest.RestResult data -> (Model a -> Cmd msg) -> (data -> String) -> Model a -> ( Model a, Cmd msg )
registered result save href model =
  { model | update = result |> Rest.done }
  |>
    (case result of
      Err _ -> Moment.nop
      Ok data ->
        {--
        -- 登録後に登録ページを表示した時に前の値が表示されるのを防ぐために空にして保存
        -- ただし、表示はそのままにしたいのでモデルは変更しない
        --}
        Moment.batch
          [ Field.reset >> save
          , always <| Location.redirectTo <| href data
          ]
    )


form : ( msg, msg ) -> List (H.Attribute msg) -> List (Content a msg) -> Content a msg
form onSubmit attrs contents model =
  let
    attributes =
      (E.onSubmit (onSubmit |> swap model))
      :: attrs
  in
    model |> Form.html (H.form attributes) contents

editForm : msg -> List (H.Attribute msg) -> List (Content a msg) -> Content a msg
editForm onSubmit attrs contents =
  let
    attributes =
      (E.onSubmit onSubmit)
      :: attrs
  in
    Form.html (H.form attributes) contents

header : List (Content a msg) -> Content a msg
header = Form.html (H.h2 [])

table : (String -> String) -> List (Cell a msg) -> Content a msg
table i18n rows model = model |>
  Form.html (H.table [])
    (rows |> List.map
      (\cell ->
        case cell of
          Cell     keys headers column -> editRow i18n keys headers (column |> swap model)
          EditCell keys headers column -> editRow i18n keys headers  column

          StaticCell headers column ->
            Form.html (H.tr [])
              [ Form.html (H.th []) headers
              , Form.html (H.td []) column
              ]

          SubCell     name keys headers column -> editRow i18n keys headers (column |> swapSub       name model)
          SubEditCell name keys headers column -> editRow i18n keys headers (column |> swapSubColumn name model)
      )
    )

editRow : (String -> String) -> List String -> List (Content a msg) -> List (Content a msg) -> Content a msg
editRow i18n keys headers column model =
  let
    state = model |> validationState keys
  in
    model
    |> Form.html (H.tr [ A.class <| validationClass state ])
      [ Form.html (H.th []) headers
      , Form.html (H.td [])
        (column ++ (state |> validationMessages i18n))
      ]

validationState : List String -> Model a -> Validate.State
validationState validationKeys model =
  if model |> Field.isEdit |> not
    then Validate.Ok
    else model |> Field.validationState validationKeys

validationClass : Validate.State -> String
validationClass state =
  case state of
    Validate.Ok    -> ""
    Validate.Err _ -> "form-error"

validationMessages : (String -> String) -> Validate.State -> List (Content a msg)
validationMessages i18n =
  Validate.errors
  >> List.map (List.singleton >> help i18n)

help : (String -> String) -> List String -> Content a msg
help translate =
  List.map (translate >> Form.text)
  >> Form.withBr
  >> Form.html (H.p [ A.class "help" ])

footer : Swap a msg -> Content a msg
footer contents model =
  model |> Form.html (H.footer []) (contents |> swap model)

swap : Model a -> ( content, content ) -> content
swap model (static,edit) =
  if model |> Field.isEdit
    then edit
    else static

swapSub : String -> Model a -> ( content, ( content, content ) ) -> content
swapSub name model (static,(subStatic,subEdit)) =
  if model |> Field.isEdit
    then
      if model |> Field.isEditSub name
        then subEdit
        else subStatic
    else static

swapSubColumn : String -> Model a -> ( content, content ) -> content
swapSubColumn name model (subStatic,subEdit) =
  if model |> Field.isEditSub name
    then subEdit
    else subStatic


edit : Content a msg
edit =
  Form.submit [ A.class "is-edit" ]
    [ always <| Fa.solid "pencil-alt" []
    , "edit.submit" |> I18n.t |> Form.text
    ]

editSub : String -> (String -> msg) -> Content a msg
editSub name msg =
  Form.button [ A.class "is-sub is-edit", E.onClick (msg name) ]
    [ always <| Fa.solid "pencil-alt" []
    ]

cancelSub : String -> (String -> msg) -> Content a msg
cancelSub name msg =
  Form.button [ A.class "is-sub is-cancel", E.onClick (msg name) ]
    [ always <| Fa.solid "times" []
    ]

save : msg -> Content a msg
save msg =
  Form.html (H.div [])
    [ commit "save"
    , cancel msg
    , error
    ]

create : Content a msg
create =
  Form.html (H.div [])
    [ commit "create"
    , error
    ]

commit : String -> Content a msg
commit = commitWith "is-save"

commitWith : String -> String -> Content a msg
commitWith class type_ model = model |>
  if model |> Field.hasError
    then
      Form.submit [ A.class "is-error" ]
        [ always <| Fa.solid "exclamation" []
        , "invalid.submit" |> I18n.t |> Form.text
        ]
    else
      case model.update of
        Just Rest.Connecting ->
          Form.button [ A.class "is-connecting" ]
            [ always <| Fa.spinner
            , type_ ++ ".connecting" |> I18n.t |> Form.text
            ]
        Just (Rest.Err _) ->
          Form.submit [ A.class "is-error" ]
            [ type_ ++ ".error" |> I18n.t |> Form.text ]
        Nothing ->
          Form.submit [ A.class class ]
            [ type_ ++ ".submit" |> I18n.t |> Form.text ]

error : Content a msg
error = errorWith ( I18n.rest, [] )

errorWith : ( (String -> String), List String ) -> Content a msg
errorWith (i18n,list) model = model |>
  case model.update of
    Just (Rest.Err error) ->
      error |> Rest.error |>
        (\m ->
          if list |> List.member m
            then m |> i18n
            else m |> I18n.rest
        )
      |> Form.badge "is-danger is-small"

    _ -> Form.none

cancel : msg -> Content a msg
cancel msg =
  Form.button [ A.class "is-cancel", E.onClick msg ]
    [ "close.submit" |> I18n.t |> Form.text ]


textBox : Modify msg -> String -> Content a msg
textBox = textInput "text" []

textBoxLarge : Modify msg -> String -> Content a msg
textBoxLarge = textInput "text" ["is-large"]

textBoxXLarge : Modify msg -> String -> Content a msg
textBoxXLarge = textInput "text" ["is-xlarge"]

telBox : Modify msg -> String -> Content a msg
telBox = textInput "tel" []

numberBox : Modify msg -> String -> Content a msg
numberBox = textInput "number" ["is-tiny"]

numberBoxLarge : Modify msg -> String -> Content a msg
numberBoxLarge = textInput "number" ["is-small"]

zipcodeBox : Modify msg -> String -> String -> Content a msg
zipcodeBox msg class = textInput "text" ["is-small", class] msg

regionBox : Modify msg -> String -> String -> Content a msg
regionBox = addressBoxWithSize ["is-small"]

localityBox : Modify msg -> String -> String -> Content a msg
localityBox = addressBoxWithSize []

addressBox : Modify msg -> String -> String -> Content a msg
addressBox = addressBoxWithSize ["is-large"]

addressBoxWithSize : List String -> Modify msg -> String -> String -> Content a msg
addressBoxWithSize size msg class name = Form.html (H.span [])
  [ name |> textInput "text" size msg
  , always <| H.input [ A.type_ "hidden", A.class class ] []
  ]

dateBox : Modify msg -> String -> Content a msg
dateBox = textInput "date" []

emailBox : Modify msg -> String -> Content a msg
emailBox = textInput "email" []

passwordBox : Modify msg -> String -> Content a msg
passwordBox = textInput "password" []

textInput : String -> List String -> Modify msg -> String -> Content a msg
textInput type_ class msg name model =
  H.input
    [ A.type_ type_
    , A.class (class |> String.join " ")
    , A.value (model |> Field.get name)
    , E.onInput (Field.Set >> msg name)
    ] []

fileBox : Modify msg -> String -> Content a msg
fileBox msg name = always <|
  H.input
    [ A.type_ "file"
    , FileReader.onFileChange (Field.Pick >> msg name)
    ] []

textarea : Modify msg -> String -> Content a msg
textarea msg name model =
  H.textarea
    [ A.rows 8
    , A.class "is-large"
    , E.onInput (Field.Set >> msg name)
    ]
    [ model |> Field.get name |> H.text ]

toggleBox : Modify msg -> String -> String -> Content a msg
toggleBox msg label name =
  Form.html (H.label [])
    [ \model ->
      H.input
        [ A.type_ "checkbox"
        , A.checked (model |> Field.is name)
        , E.onClick (Field.Toggle |> msg name)
        ] []
    , label |> Form.text
    ]

boolList : Modify msg -> (Bool -> String) -> String -> Content a msg
boolList = boolListWithClass ""

boolListInline : Modify msg -> (Bool -> String) -> String -> Content a msg
boolListInline = boolListWithClass "is-inline"

boolListWithClass : String -> Modify msg -> (Bool -> String) -> String -> Content a msg
boolListWithClass class msg i18n name model =
  let
    value = model |> Field.is name
  in
    [ True, False ]
    |> options i18n
    |> List.map
      (\(option,label) -> H.li []
        [ H.label []
          [ H.input
            [ A.type_ "radio"
            , A.checked (value == option)
            , E.onClick (Field.Put option |> msg name)
            ] []
          , " "   |> H.text
          , label |> H.text
          ]
        ]
      )
    |> H.ul [ A.class class ]

radioList : Modify msg -> (String -> String) -> String -> Content a msg
radioList = radioListWithClass ""

radioListInline : Modify msg -> (String -> String) -> String -> Content a msg
radioListInline = radioListWithClass "is-inline"

radioListWithClass : String -> Modify msg -> (String -> String) -> String -> Content a msg
radioListWithClass class msg i18n name model =
  let
    value = model |> Field.value name
  in
    model
    |> Field.options name
    |> options i18n
    |> List.map
      (\(option,label) -> H.li []
        [ H.label []
          [ H.input
            [ A.type_ "radio"
            , A.checked (value == Just option)
            , E.onClick (Field.Select (Just option) |> msg name)
            ] []
          , " "   |> H.text
          , label |> H.text
          ]
        ]
      )
    |> H.ul [ A.class class ]

checkList : Modify msg -> (String -> String) -> String -> Content a msg
checkList = checkListWithClass ""

checkListInline : Modify msg -> (String -> String) -> String -> Content a msg
checkListInline = checkListWithClass "is-inline"

checkListWithClass : String -> Modify msg -> (String -> String) -> String -> Content a msg
checkListWithClass class msg i18n name model =
  let
    values = model |> Field.values name
  in
    model
    |> Field.options name
    |> options i18n
    |> List.map
      (\(option,label) -> H.li []
        [ H.label []
          [ H.input
            [ A.type_ "checkbox"
            , A.checked (values |> List.member option)
            , E.onClick (Field.Check option |> msg name)
            ] []
          , " "   |> H.text
          , label |> H.text
          ]
        ]
      )
    |> H.ul [ A.class class ]

select : Modify msg -> List ( String, String ) -> String -> Content a msg
select msg values name model =
  let
    value = model |> Field.get name
  in
    values
    |> List.append [("", "select.empty" |> I18n.t)]
    |> List.map
      (\(option,label) ->
        H.option
          [ A.value option
          , A.selected (option == value)
          ]
          [ H.text label ]
      )
    |> H.select [ E.onInput (Field.Set >> msg name) ]


options : (value -> String) -> List value -> List ( value, String )
options toLabel = List.map (\value -> (value, value |> toLabel))
