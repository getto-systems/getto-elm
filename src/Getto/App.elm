module Getto.App exposing
  ( Msg
  , init
  , update
  , info_
  )

import Getto.Model.Opts as Opts exposing ( Opts )
import Getto.Model.Flags exposing ( Flags )
import Getto.Model.GeneralInfo exposing ( GeneralInfo )
import Getto.Model.Credential as Credential

import Getto.Moment as Moment
import Getto.Auth as Auth
import Getto.Auth.Decode as AuthDecode
import Getto.Storage as Storage
import Getto.Location as Location
import Getto.Config as Config
import Getto.Env as Env
import Getto.Moment as Moment
import Getto.Json as Json

import Http
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Extra exposing ( (|:) )
import Date.Extra

import Focus exposing ( (=>) )


info_       = Focus.create .info       (\f model -> { model | info       = model.info       |> f })
credential_ = Focus.create .credential (\f model -> { model | credential = model.credential |> f })
token_      = Focus.create .token      (\f model -> { model | token      = model.token      |> f })


type Msg full
  = Done (Opts.RenewResult full)

type alias AppInfo full limited = GeneralInfo {} full limited
type alias Init full limited model = AppInfo full limited -> ( model, Cmd (Msg full) )

init : Credential.AuthMethod -> (Msg full -> msg) -> Init full limited model -> Opts full limited -> Flags -> ( model, Cmd msg )
init authMethod msg func opts flags =
  { application =
    { version   = opts.version
    , copyright = opts.copyright
    }
  , storage = flags.storage
  , api =
    { token = Nothing
    }
  , page       = flags.page
  , project    = flags.project
  , credential =
    { authMethod   = authMethod
    , token        = Nothing
    , rememberMe   = True
    , previousPath = Nothing
    }
  }
  |> initCredential opts
  |> Moment.andThen func
  >> Moment.map msg


initCredential : Opts full limited -> AppInfo full limited -> ( AppInfo full limited, Cmd (Msg full) )
initCredential opts model =
  case model.credential.authMethod of
    Credential.Public ->
      let
        storage =
          model.storage.global.credential
          |> Decode.decodeValue (opts.decoder.full |> fullStorage)
          |> Result.toMaybe
      in
        model
        |> Focus.update credential_
          (\credential ->
            { credential
            | token        = Just Credential.NoToken
            , rememberMe   = storage |> Maybe.map .rememberMe |> Maybe.withDefault True
            , previousPath = storage |> Maybe.andThen .previousPath
            }
          )
        |> Moment.batch
          (case storage |> Maybe.map .info of
            Nothing -> []
            Just _  -> [ Auth.previousPath >> Location.redirectTo ]
          )

    Credential.LimitedAuth authType ->
      let
        storage =
          model.storage.global.credential
          |> Decode.decodeValue (opts.decoder.limited |> limitedStorage authType)
          |> Result.toMaybe
      in
        { model
        | api =
          { token = storage |> Maybe.map .info |> Maybe.map .token
          }
        }
        |> Focus.update credential_
          (case storage of
            Nothing -> identity
            Just storage ->
              (\credential ->
                { credential
                | token        = storage.info |> Credential.LimitedToken |> Just
                , rememberMe   = storage.rememberMe
                , previousPath = storage.previousPath
                }
              )
          )
        |> Moment.nop

    Credential.FullAuth config ->
      let
        storage =
          model.storage.global.credential
          |> Decode.decodeValue (opts.decoder.full |> fullStorage)
          |> Result.toMaybe

        renewRequired =
          case storage |> Maybe.map .info of
            Nothing -> False
            Just token ->
              case
                ( model.page.loadAt |> Date.Extra.fromIsoString
                , token.issued_at   |> Date.Extra.fromIsoString
                )
              of
                (Just loadAt, Just issued_at) ->
                  Date.Extra.diff Date.Extra.Hour issued_at loadAt > config.expireHours
                _ -> True
      in
        { model
        | api =
          { token = storage |> Maybe.map .info |> Maybe.map .token
          }
        }
        |> Focus.update credential_
          (case storage of
            Nothing -> identity
            Just storage ->
              (\credential ->
                { credential
                | token        = storage.info |> Credential.FullToken |> Just
                , rememberMe   = storage.rememberMe
                , previousPath = model.page.query |> Just
                }
              )
          )
        |> Moment.batch
          (if renewRequired
            then [ .api >> opts.renew >> Http.send Done ]
            else []
          )


type alias FullStorage a =
  { info         : Credential.Full a
  , rememberMe   : Bool
  , previousPath : Maybe String
  }

fullStorage : Decode.Decoder a -> Decode.Decoder (FullStorage a)
fullStorage decoder =
  Decode.succeed FullStorage
  |: (Decode.at ["full"]         (full decoder))
  |: (Decode.at ["rememberMe"]    Decode.bool)
  |: (Decode.at ["previousPath"] (Decode.maybe Decode.string))

type alias Full a =
  { account   : a
  , token     : String
  , issued_at : String
  }

full : Decode.Decoder a -> Decode.Decoder (Credential.Full a)
full decoder =
  Decode.succeed Full
  |: (Decode.at ["token"]    (AuthDecode.jwt decoder))
  |: (Decode.at ["token"]     Decode.string)
  |: (Decode.at ["issued_at"] Decode.string)


type alias LimitedStorage a =
  { info         : Credential.Limited a
  , rememberMe   : Bool
  , previousPath : Maybe String
  }

limitedStorage : String -> Decode.Decoder a -> Decode.Decoder (LimitedStorage a)
limitedStorage authType decoder =
  Decode.succeed LimitedStorage
  |: (Decode.at ["limited." ++ authType] (limited decoder))
  |: (Decode.at ["rememberMe"]            Decode.bool)
  |: (Decode.at ["previousPath"]         (Decode.maybe Decode.string))

type alias Limited a =
  { account : a
  , token   : String
  }

limited : Decode.Decoder a -> Decode.Decoder (Credential.Limited a)
limited decoder =
  Decode.succeed Limited
  |: (Decode.at ["token"] (AuthDecode.jwt decoder))
  |: (Decode.at ["token"]  Decode.string)


update : Msg full -> GeneralInfo m full limited -> ( GeneralInfo m full limited, Cmd (Msg full) )
update msg model =
  case msg of
    Done result ->
      case result of
        Err _ -> model |> Moment.batch [ always (Auth.loginPath |> Location.redirectTo) ]
        Ok token ->
          model
          |> Focus.set (credential_ => token_) (token |> Credential.FullToken |> Just)
          |> Moment.nop
