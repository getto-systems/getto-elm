module Getto.Model.Page exposing
  ( Page
  )

import Getto.Location as Location

type alias Page =
  { name   : String
  , path   : String
  , search : Location.Search
  , query  : String
  , loadAt : String
  }
