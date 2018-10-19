module Getto.Model.Credential exposing
  ( Credential
  , AuthMethod(..)
  , FullConfig
  , LimitedConfig
  , LimitedStatus(..)
  , Token(..)
  , Reset
  , Full
  , FullInfo
  , Limited
  , LimitedInfo
  )

type alias Credential account =
  { authMethod   : AuthMethod
  , token        : Maybe (Token account)
  , rememberMe   : Bool
  , previousPath : Maybe String
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
  , info    : FullInfo
  , token   : String
  }

type alias FullInfo =
  { issued_at : String
  }

type alias Limited =
  { info  : LimitedInfo
  , token : String
  }

type alias LimitedInfo =
  { status : LimitedStatus
  }
