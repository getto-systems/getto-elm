module Getto.Main exposing
  ( Base
  , Info
  , Msg(..)
  , init
  , update
  , toggleMenu
  )
import Getto
import Getto.App as App
import Getto.Storage as Storage
import Getto.Nav as Nav

type alias Base info m =
  { m
  | info : Info info
  }

type alias Info info = Getto.GeneralInfo
  { info
  | menu : Nav.Menu
  }

type alias MainInfo = Info {}

type Msg
  = Nop
  | ToggleMenu String


init : Getto.Opts -> Getto.Flags -> (MainInfo -> ( model, Cmd msg )) -> ( model, Cmd msg )
init opts flags func = App.init opts flags <|
  \base ->
    let
      info =
        { application = base.application
        , storage     = base.storage
        , api         = base.api
        , page        = base.page
        , project     = base.project
        , credential  = base.credential
        , menu        = flags.storage.global.menu |> Nav.decode
        }

      (model,cmd) = info |> func
    in
      model ! [ cmd ]

update : Msg -> Base info m -> ( Base info m, Cmd Msg )
update msg model =
  case msg of
    Nop -> model ! []

    ToggleMenu name ->
      let
        info = model.info
        (menu,cmd) = info.menu |> toggleMenu name
      in
        { model | info = { info | menu = menu } } ! [ cmd ]

toggleMenu : String -> Nav.Menu -> ( Nav.Menu, Cmd msg )
toggleMenu name menu =
  let
    collapsed =
      if menu.collapsed |> List.member name
        then menu.collapsed |> List.filter (\group -> group /= name)
        else name :: menu.collapsed
    newMenu = { menu | collapsed = collapsed }
  in
    newMenu ! [ newMenu |> Nav.encode |> Storage.saveMenu ]
