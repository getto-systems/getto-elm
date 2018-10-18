module Getto.Model.Credential exposing
  ( Credential
  , AuthMethod(..)
  , FullConfig
  , LimitedConfig
  , LimitedStatus(..)
  , Token(..)
  , Full
  , FullInfo
  , Limited
  , LimitedInfo
  )

type alias Credential full limited =
  { authMethod   : AuthMethod
  , token        : Maybe (Token full limited)
  , rememberMe   : Bool
  , previousPath : Maybe String
  }

type AuthMethod
  = Public
  | FullAuth FullConfig
  | LimitedAuth LimitedConfig

type alias FullConfig =
  { expireHours : Int
  }

type alias LimitedConfig =
  { name : String
  , status : LimitedStatus
  }

type LimitedStatus
  = LimitedRegistered
  | LimitedUnregistered


type Token full limited
  = NoToken
  | FullToken    (Full full)
  | LimitedToken (Limited limited)

type alias Full a =
  { account : a
  , info    : FullInfo
  , token   : String
  }

type alias FullInfo =
  { issued_at : String
  }

type alias Limited a =
  { account : a
  , info    : LimitedInfo
  , token   : String
  }

type alias LimitedInfo =
  { status : LimitedStatus
  }
