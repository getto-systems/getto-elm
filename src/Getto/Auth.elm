module Getto.Auth exposing
  ( clear
  , logout
  , register
  , login
  , rememberMe
  , loginPath
  , limitedVerifyPath
  , limitedSetupPath
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
issuedAt_     = Focus.create .issuedAt     (\f model -> { model | issuedAt     = model.issuedAt     |> f })


clear : GeneralInfo m account -> ( GeneralInfo m account, Cmd msg )
clear model =
  { model | api = { token = Nothing } }
  |> Focus.set (credential_ => token_) Nothing
  |> Moment.batch [ save ]

logout : GeneralInfo m account -> ( GeneralInfo m account, Cmd msg )
logout =
  clear >> Moment.andThen
    ( Focus.set (credential_ => previousPath_) Nothing
      >> Moment.batch [ always (loginPath |> Location.redirectTo) ]
    )


register : Credential.Token account -> GeneralInfo m account -> ( GeneralInfo m account, Cmd msg )
register token =
  Focus.set (credential_ => token_) (Just token)
  >>
    (\model ->
      model |> Focus.set (credential_ => issuedAt_) (model.page.loadAt |> Just)
    )
  >> Moment.batch [ save ]

login : Credential.Token account -> GeneralInfo m account -> ( GeneralInfo m account, Cmd msg )
login token =
  register token
  >> Moment.andThen
    (Moment.batch
      (case token of
        Credential.NoToken      -> []
        Credential.ResetToken _ -> []
        Credential.FullToken  _ -> [ previousPath >> Location.redirectTo ]
        Credential.LimitedToken name token ->
          [ \model ->
            case model.credential.authMethod of
              Credential.Public ->
                case token.info.status of
                  Credential.LimitedRegistered   -> name |> limitedVerifyPath |> Location.redirectTo
                  Credential.LimitedUnregistered -> name |> limitedSetupPath  |> Location.redirectTo
              _ -> Cmd.none
          ]
      )
    )


save : GeneralInfo m account -> Cmd msg
save = .credential >> encode >> Storage.saveCredential

encode : Credential account -> Encode.Value
encode credential = Encode.object
  [ (credential.token |> key, credential.token        |> defaultNull encodeToken)
  , ("rememberMe",            credential.rememberMe   |> Encode.bool)
  , ("previousPath",          credential.previousPath |> defaultNull Encode.string)
  , ("issuedAt",              credential.issuedAt     |> defaultNull Encode.string)
  ]

key : Maybe (Credential.Token account) -> String
key token =
  case token of
    Just (Credential.ResetToken        _) -> "reset"
    Just (Credential.FullToken         _) -> "full"
    Just (Credential.LimitedToken name _) -> "limited." ++ name
    _ -> "no-token"

encodeToken : Credential.Token account -> Encode.Value
encodeToken token =
  case token of
    Credential.NoToken                -> Encode.null
    Credential.ResetToken        info -> info.token |> Encode.string
    Credential.FullToken         info -> info.token |> Encode.string
    Credential.LimitedToken name info -> info.token |> Encode.string

defaultNull f = Maybe.map f >> Maybe.withDefault Encode.null


rememberMe : Bool -> GeneralInfo m account -> GeneralInfo m account
rememberMe = Focus.set (credential_ => rememberMe_)


loginPath : String
loginPath = Env.pageRoot ++ Config.loginPath

limitedVerifyPath : String -> String
limitedVerifyPath name =
  Env.pageRoot ++ (name |> Config.limitedVerifyPath)

limitedSetupPath : String -> String
limitedSetupPath name =
  Env.pageRoot ++ (name |> Config.limitedSetupPath)

previousPath : GeneralInfo m account -> String
previousPath = .credential >> .previousPath >> Maybe.withDefault (Env.pageRoot ++ Config.topPath)
