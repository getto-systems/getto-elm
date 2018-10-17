module Getto.App exposing
  ( init
  , info_
  , info_credential_
  )
import Getto
import Getto.Env as Env
import Getto.Location as Location
import Getto.Auth as Auth

import Getto.Moment as Moment
import Getto.Json as Json

import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode

import Focus exposing ( (=>) )

init : Getto.Opts -> Getto.Flags -> (Getto.Info -> ( model, Cmd msg )) -> ( model, Cmd msg )
init opts flags func =
  let
    (credential,cmd) = flags |> Auth.init opts
  in
    ( { application =
        { version   = opts.version
        , copyright = "GETTO systems"
        }
      , storage = flags.storage
      , api =
        { token = credential.token
        }
      , page       = flags.page
      , project    = flags.project
      , credential = credential
      }
    , cmd
    )
    |> Moment.andThen func


info_       = Focus.create .info       (\f model -> { model | info       = model.info       |> f })
credential_ = Focus.create .credential (\f model -> { model | credential = model.credential |> f })

info_credential_ = info_ => credential_
