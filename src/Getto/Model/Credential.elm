module Getto.Model.Credential exposing
  ( Credential
  , AuthMethod(..)
  , FullConfig
  , LimitedConfig
  , LimitedStatus(..)
  , Token(..)
  , Reset
  , Full
  , Limited
  , LimitedInfo
  , account
  )

type alias Credential account =
  { authMethod   : AuthMethod
  , token        : Maybe (Token account)
  , rememberMe   : Bool
  , previousPath : Maybe String
  , issuedAt     : Maybe String
  }

type AuthMethod
  = Public
  | ResetAuth ResetConfig
  | FullAuth FullConfig
  | LimitedAuth String LimitedConfig

type alias ResetConfig =
  { key : String
  }

type alias FullConfig =
  { expireHours : Int
  }

type alias LimitedConfig =
  { status : LimitedStatus
  }

type LimitedStatus
  = LimitedRegistered
  | LimitedUnregistered


type Token account
  = NoToken
  | ResetToken Reset
  | FullToken (Full account)
  | LimitedToken String Limited

type alias Reset =
  { token : String
  }

type alias Full a =
  { account : a
  , token   : String
  }

type alias Limited =
  { info  : LimitedInfo
  , token : String
  }

type alias LimitedInfo =
  { status : LimitedStatus
  }

account : Credential account -> Maybe account
account credential =
  case credential.token of
    Just (FullToken token) -> Just token.account
    _ -> Nothing
