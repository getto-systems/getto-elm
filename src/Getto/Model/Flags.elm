module Getto.Model.Flags exposing
  ( Flags
  )

import Getto.Model.Storage exposing ( Storage )
import Getto.Model.Page exposing ( Page )
import Getto.Model.Project exposing ( Project )

type alias Flags =
  { storage : Storage
  , page    : Page
  , project : Project
  }
