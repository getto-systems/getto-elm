module Getto.Auth exposing
  ( Token
  , token
  , init
  , logout
  , authenticated
  )
import Getto
import Getto.Storage as Storage
import Getto.Location as Location
import Getto.Config as Config
import Getto.Env as Env
import Getto.Moment as Moment
import Getto.Json as Json

import Result
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Date.Extra
import Base64

type alias Token =
  { token : String
  , info : Info
  }

type alias Info =
  { id : Int
  , role : List String
  , login_id : String
  , renewedAt : String
  }

token : Decode.Decoder Token
token =
  Decode.succeed Token
  |: (Decode.at ["jwt"] Decode.string)
  |: (Decode.at ["jwt"] jwt)

jwt : Decode.Decoder Info
jwt =
  Decode.string
  |> Decode.andThen
    (\jwt ->
      let
        padding token =
          let
            length = token |> String.length
            fullLength = length + ((4 - (length % 4)) % 4)
          in
            token |> String.padRight fullLength '='

        token =
          case jwt |> String.split "." of
            _ :: payload :: _ ->
              payload
              |> padding
              |> Base64.decode
              |> Result.mapError toString
              |> Result.mapError ((++) ("base64 error [" ++ payload ++ "]: "))
              |> Result.andThen (Decode.decodeString info)
            _ ->
              Err ("jwt mismatch " ++ (jwt |> String.split "." |> String.join(" ")))
      in
        case token of
          Ok val      -> Decode.succeed val
          Err message -> Decode.fail message
    )

info : Decode.Decoder Info
info =
  Decode.succeed Info
  |: (Decode.at ["sub"] Decode.int)
  |: (Decode.at ["aud"] (Decode.list Decode.string))
  |: (Decode.at ["login_id"] Decode.string)
  |: (Decode.at ["renewedAt"] Decode.string)


init : Getto.Opts -> Getto.Flags -> ( Getto.Credential, Cmd msg )
init opts flags =
  let
    expireHours = 20

    default = Maybe.withDefault

    storage = flags.storage.global.credential
    credential =
      { login_id   = storage |> Json.decodeValue ["login_id"]   Decode.string              |> default ""
      , rememberMe = storage |> Json.decodeValue ["rememberMe"] Decode.bool                |> default True
      , role       = storage |> Json.decodeValue ["role"]      (Decode.list Decode.string) |> default []
      , token      = storage |> Json.decodeValue ["token"]      Decode.string
      , oldToken   = storage |> Json.decodeValue ["oldToken"]   Decode.string
      , previous   = storage |> Json.decodeValue ["previous"]   Decode.string
      , renewedAt  = storage |> Json.decodeValue ["renewedAt"]  Decode.string
      , authRequired = opts.authRequired
      }

    renewRequired =
      case credential.token of
        Nothing -> False
        Just _ ->
          case
            ( flags.page.loadAt                  |> Date.Extra.fromIsoString
            , credential.renewedAt |> default "" |> Date.Extra.fromIsoString
            )
          of
            (Just loadAt, Just renewedAt) ->
              Date.Extra.diff Date.Extra.Hour renewedAt loadAt > expireHours
            _ -> True

    token =
      if renewRequired
        then Nothing
        else credential.token

    oldToken =
      if renewRequired && credential.rememberMe
        then credential.token
        else credential.oldToken

    previous =
      if credential.authRequired
        then Just flags.page.query
        else credential.previous
  in
    { credential
    | token = token
    , oldToken = oldToken
    , previous = previous
    } |> Moment.batch
      (case (opts.authRequired, token) of
        (False, Just _) ->
          [ previousPath >> Location.redirectTo
          ]

        (True, Nothing) ->
          [ save
          , loginPath >> Location.redirectTo
          ]

        _ -> []
      )


logout : Getto.Credential -> ( Getto.Credential, Cmd msg )
logout =
  clear >> resetPrevious >> Moment.batch
    [ save
    , loginPath >> Location.redirectTo
    ]

authenticated : Result error Token -> Getto.Credential -> ( Getto.Credential, Cmd msg )
authenticated result credential =
  (case result of
    Err _ -> credential |> clear
    Ok auth ->
      { credential
      | role = auth.info.role
      , login_id = auth.info.login_id
      , token = Just auth.token
      , oldToken = Nothing
      , renewedAt = Just auth.info.renewedAt
      }
  ) |> Moment.batch
    [ save
    , case result of
      Ok _  -> previousPath >> Location.redirectTo
      Err _ -> always Cmd.none
    ]

clear : Getto.Credential -> Getto.Credential
clear credential =
  { credential
  | role = []
  , login_id = ""
  , token = Nothing
  , oldToken = Nothing
  , renewedAt = Nothing
  }

resetPrevious : Getto.Credential -> Getto.Credential
resetPrevious credential = { credential | previous = Nothing }


save : Getto.Credential -> Cmd msg
save = encode >> Storage.saveCredential

encode : Getto.Credential -> Encode.Value
encode credential = Encode.object
  [ ("login_id",   credential.login_id   |> Encode.string)
  , ("rememberMe", credential.rememberMe |> Encode.bool)
  , ("role",       credential.role       |> List.map Encode.string |> Encode.list)
  , ("token",      credential.token      |> Maybe.map Encode.string |> Maybe.withDefault Encode.null)
  , ("oldToken",   credential.oldToken   |> Maybe.map Encode.string |> Maybe.withDefault Encode.null)
  , ("previous",   credential.previous   |> Maybe.map Encode.string |> Maybe.withDefault Encode.null)
  , ("renewedAt",  credential.renewedAt  |> Maybe.map Encode.string |> Maybe.withDefault Encode.null)
  ]


loginPath : Getto.Credential -> String
loginPath credential =
  String.append Env.pageRoot <|
    if credential.oldToken == Nothing
      then Config.loginPath
      else Config.renewPath

previousPath : Getto.Credential -> String
previousPath = .previous >> Maybe.withDefault Env.pageRoot
