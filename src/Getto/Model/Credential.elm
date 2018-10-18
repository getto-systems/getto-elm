module Getto.Model.Credential exposing
  ( Credential
  , AuthMethod(..)
  , FullConfig
  , Token(..)
  , Full
  , Limited
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
  | LimitedAuth String

type alias FullConfig =
  { expireHours : Int
  }

type Token full limited
  = NoToken
  | FullToken    (Full full)
  | LimitedToken (Limited limited)

type alias Full a =
  { account   : a
  , token     : String
  , issued_at : String
  }

type alias Limited a =
  { account : a
  , token   : String
  }
