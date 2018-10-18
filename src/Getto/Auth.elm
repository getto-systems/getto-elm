module Getto.Auth exposing
  ( clear
  , logout
  , login
  , rememberMe
  , loginPath
  , previousPath
  )

import Getto.Model.GeneralInfo exposing ( GeneralInfo )
import Getto.Model.Credential as Credential exposing ( Credential )

import Getto.Storage as Storage
import Getto.Location as Location
import Getto.Config as Config
import Getto.Env as Env
import Getto.Moment as Moment

import Json.Encode as Encode

import Focus exposing ( (=>) )


credential_   = Focus.create .credential   (\f model -> { model | credential   = model.credential   |> f })
token_        = Focus.create .token        (\f model -> { model | token        = model.token        |> f })
previousPath_ = Focus.create .previousPath (\f model -> { model | previousPath = model.previousPath |> f })
rememberMe_   = Focus.create .rememberMe   (\f model -> { model | rememberMe   = model.rememberMe   |> f })


clear : GeneralInfo m full limited -> GeneralInfo m full limited
clear model =
  { model | api = { token = Nothing } }
  |> Focus.set (credential_ => token_) Nothing

logout : GeneralInfo m full limited -> ( GeneralInfo m full limited, Cmd msg )
logout =
  clear
  >> Focus.set (credential_ => previousPath_) Nothing
  >> Moment.batch
    [ save
    , always (loginPath |> Location.redirectTo)
    ]


login : Credential.Token full limited -> GeneralInfo m full limited -> ( GeneralInfo m full limited, Cmd msg )
login token =
  Focus.set (credential_ => token_) (Just token)
  >> Moment.batch
    (case token of
      Credential.NoToken -> []
      Credential.FullToken _ ->
        [ save
        , previousPath >> Location.redirectTo
        ]
      Credential.LimitedToken _ ->
        [ save
        , always (verifyPath |> Location.redirectTo)
        ]
    )


save : GeneralInfo m full limited -> Cmd msg
save = .credential >> encode >> Storage.saveCredential

encode : Credential full limited -> Encode.Value
encode credential = Encode.object
  [ (credential.authMethod |> key, credential.token        |> defaultNull encodeToken)
  , ("rememberMe",                 credential.rememberMe   |> Encode.bool)
  , ("previousPath",               credential.previousPath |> defaultNull Encode.string)
  ]

key : Credential.AuthMethod -> String
key authMethod =
  case authMethod of
    Credential.Public           -> "full"
    Credential.FullAuth _       -> "full"
    Credential.LimitedAuth name -> "limited." ++ name

encodeToken : Credential.Token full limited -> Encode.Value
encodeToken token =
  case token of
    Credential.NoToken           -> Encode.null
    Credential.FullToken    info -> info.token |> Encode.string
    Credential.LimitedToken info -> info.token |> Encode.string

defaultNull f = Maybe.map f >> Maybe.withDefault Encode.null


rememberMe : Bool -> GeneralInfo m full limited -> GeneralInfo m full limited
rememberMe = Focus.set (credential_ => rememberMe_)


loginPath : String
loginPath = Env.pageRoot ++ Config.loginPath

verifyPath : String
verifyPath = Env.pageRoot ++ Config.verifyPath

previousPath : GeneralInfo m full limited -> String
previousPath = .credential >> .previousPath >> Maybe.withDefault Env.pageRoot
