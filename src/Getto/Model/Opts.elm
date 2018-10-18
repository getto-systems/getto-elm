module Getto.Model.Opts exposing
  ( Opts
  , RenewRequest
  , RenewResult
  )

import Getto.Model.Credential as Credential

import Getto.Rest as Rest

import Json.Decode as Decode

type alias Opts full =
  { version   : String
  , copyright : String
  , renew     : RenewRequest full
  , decoder :
    { full    : Decode.Decoder full
    }
  }

type alias RenewRequest full = Rest.Request    (Credential.Full full)
type alias RenewResult  full = Rest.RestResult (Credential.Full full)
