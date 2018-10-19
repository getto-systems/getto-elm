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

type alias Base m info account =
  { m
  | info : Info info account
  }

type alias Info info account = GeneralInfo
  { info
  | menu : Nav.Menu
  }
  account

type alias MainInfo account = Info {} account

type Msg account
  = Super (App.Msg account)
  | ToggleMenu String


type alias Init account model msg = MainInfo account -> ( model, Cmd msg )

init : Credential.AuthMethod -> (Msg account -> msg) -> Init account model msg -> Opts account -> Flags -> ( model, Cmd msg )
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

update : Msg account -> Base m info account -> ( Base m info account, Cmd (Msg account) )
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
