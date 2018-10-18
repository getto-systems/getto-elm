module Getto.Model.Storage exposing
  ( Storage
  )

import Getto.Storage as Storage

import Json.Encode as Encode

type alias Storage =
  { global :
    { credential : Encode.Value
    , menu       : Encode.Value
    , terminal   : Encode.Value
    }
  , page : Storage.Page
  }
