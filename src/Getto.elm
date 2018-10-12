module Getto exposing
  ( Flags
  , Opts
  , GeneralInfo
  , Info
  , Application
  , Storage
  , Api
  , Page
  , Project
  , Credential
  )
import Getto.Location as Location
import Getto.Storage as Storage

import Json.Encode as Encode

type alias Flags =
  { storage : Storage
  , page    : Page
  , project : Project
  }

type alias Opts =
  { version      : String
  , authRequired : Bool
  }

type alias GeneralInfo info =
  { info
  | application : Application
  , storage     : Storage
  , api         : Api
  , page        : Page
  , project     : Project
  , credential  : Credential
  }

type alias Info = GeneralInfo {}

type alias Application =
  { version   : String
  , copyright : String
  }

type alias Storage =
  { global :
    { credential : Encode.Value
    , menu       : Encode.Value
    , terminal   : Encode.Value
    }
  , page : Storage.Page
  }

type alias Api =
  { token : Maybe String
  }

type alias Page =
  { name   : String
  , path   : String
  , search : Location.Search
  , query  : String
  , loadAt : String
  }

type alias Project =
  { name     : String
  , company  : String
  , title    : String
  , subTitle : String
  }

type alias Credential =
  { login_id     : String
  , rememberMe   : Bool
  , role         : List String
  , token        : Maybe String
  , oldToken     : Maybe String
  , previous     : Maybe String
  , renewedAt    : Maybe String
  , authRequired : Bool
  }
