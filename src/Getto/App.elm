module Getto.App exposing
  ( Msg
  , init
  , update
  , fullInfo
  , limitedInfo
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
          (case storage of
            Nothing -> []
            Just _  -> [ Auth.previousPath >> Location.redirectTo ]
          )

    Credential.LimitedAuth name config ->
      let
        storage =
          model.storage.global.credential
          |> Decode.decodeValue (opts.decoder.limited |> limitedStorage name)
          |> Result.toMaybe
      in
        case storage of
          Nothing -> model |> Moment.batch [ always (Auth.loginPath |> Location.redirectTo) ]
          Just storage ->
            { model | api = { token = storage.limited.token |> Just } }
            |> Focus.update credential_
              (\credential ->
                { credential
                | token        = storage.limited |> Credential.LimitedToken name |> Just
                , rememberMe   = storage.rememberMe
                , previousPath = storage.previousPath
                }
              )
            |> Moment.batch
              (case (config.status, storage.limited.info.status) of
                (Credential.LimitedRegistered, Credential.LimitedUnregistered) ->
                  [ always (name |> Auth.limitedSetupPath |> Location.redirectTo) ]
                (Credential.LimitedUnregistered, Credential.LimitedRegistered) ->
                  [ always (name |> Auth.limitedVerifyPath |> Location.redirectTo) ]
                _ -> []
              )

    Credential.FullAuth config ->
      let
        storage =
          model.storage.global.credential
          |> Decode.decodeValue (opts.decoder.full |> fullStorage)
          |> Result.toMaybe
      in
        case storage of
          Nothing -> model |> Moment.batch [ always (Auth.loginPath |> Location.redirectTo) ]
          Just storage ->
            let
              renewRequired =
                case
                  ( model.page.loadAt |> Date.Extra.fromIsoString
                  , storage.full.info.issued_at |> Date.Extra.fromIsoString
                  )
                of
                  (Just loadAt, Just issued_at) ->
                    Date.Extra.diff Date.Extra.Hour issued_at loadAt > config.expireHours
                  _ -> True
            in
              { model | api = { token = storage.full.token |> Just } }
              |> Focus.update credential_
                (\credential ->
                  { credential
                  | token        = storage.full |> Credential.FullToken |> Just
                  , rememberMe   = storage.rememberMe
                  , previousPath = model.page.query |> Just
                  }
                )
              |> Moment.batch
                (if renewRequired
                  then [ .api >> opts.renew >> Http.send Done ]
                  else []
                )


type alias FullStorage a =
  { full         : Credential.Full a
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
  { account : a
  , info    : Credential.FullInfo
  , token   : String
  }

full : Decode.Decoder a -> Decode.Decoder (Credential.Full a)
full decoder =
  Decode.succeed Full
  |: (AuthDecode.jwt decoder)
  |: (AuthDecode.jwt fullInfo)
  |: Decode.string

type alias FullInfo =
  { issued_at : String
  }

fullInfo : Decode.Decoder FullInfo
fullInfo =
  Decode.succeed FullInfo
  |: (Decode.at ["iat"] Decode.string)


type alias LimitedStorage a =
  { limited      : Credential.Limited a
  , rememberMe   : Bool
  , previousPath : Maybe String
  }

limitedStorage : String -> Decode.Decoder a -> Decode.Decoder (LimitedStorage a)
limitedStorage name decoder =
  Decode.succeed LimitedStorage
  |: (Decode.at ["limited." ++ name] (limited decoder))
  |: (Decode.at ["rememberMe"]        Decode.bool)
  |: (Decode.at ["previousPath"]     (Decode.maybe Decode.string))

type alias Limited a =
  { account : a
  , info    : Credential.LimitedInfo
  , token   : String
  }

limited : Decode.Decoder a -> Decode.Decoder (Credential.Limited a)
limited decoder =
  Decode.succeed Limited
  |: (AuthDecode.jwt decoder)
  |: (AuthDecode.jwt limitedInfo)
  |: Decode.string

type alias LimitedInfo =
  { status : String
  }

limitedInfo : Decode.Decoder Credential.LimitedInfo
limitedInfo =
  Decode.succeed LimitedInfo
  |: (Decode.at ["aud"] Decode.string)
  |> Decode.andThen
    (\info ->
      Decode.succeed
        { status =
          if info.status == "registered"
            then Credential.LimitedRegistered
            else Credential.LimitedUnregistered
        }
    )


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
