module Getto.Main exposing
  ( Base
  , Info
  , Msg(..)
  , init
  , update
  , toggleMenu
  )
import Getto.Model.Opts exposing ( Opts )
import Getto.Model.Flags exposing ( Flags )
import Getto.Model.GeneralInfo exposing ( GeneralInfo )
import Getto.Model.Credential as Credential

import Getto.App as App
import Getto.Moment as Moment
import Getto.Storage as Storage
import Getto.Nav as Nav

type alias Base m info full =
  { m
  | info : Info info full
  }

type alias Info info full = GeneralInfo
  { info
  | menu : Nav.Menu
  }
  full

type alias MainInfo full = Info {} full

type Msg full
  = Super (App.Msg full)
  | ToggleMenu String


type alias Init full model msg = MainInfo full -> ( model, Cmd msg )

init : Credential.AuthMethod -> (Msg full -> msg) -> Init full model msg -> Opts full -> Flags -> ( model, Cmd msg )
init authMethod msg func = App.init authMethod (Super >> msg) <|
  \info ->
    { application = info.application
    , storage     = info.storage
    , api         = info.api
    , page        = info.page
    , project     = info.project
    , credential  = info.credential
    , menu        = info.storage.global.menu |> Nav.decode
    }
    |> func

update : Msg full -> Base m info full -> ( Base m info full, Cmd (Msg full) )
update msg model =
  case msg of
    Super msg -> model |> Moment.update App.info_ (App.update msg) |> Moment.map Super

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
