module Getto.Model.Opts exposing
  ( Opts
  , RenewRequest
  , RenewResult
  )

import Getto.Model.Credential as Credential

import Getto.Rest as Rest

import Json.Decode as Decode

type alias Opts account =
  { version   : String
  , copyright : String
  , renew     : RenewRequest account
  , decoder :
    { account : Decode.Decoder account
    }
  }

type alias RenewRequest account = Rest.Request    (Credential.Full account)
type alias RenewResult  account = Rest.RestResult (Credential.Full account)
