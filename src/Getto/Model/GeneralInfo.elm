module Getto.Model.GeneralInfo exposing
  ( GeneralInfo
  )

import Getto.Model.Application exposing ( Application )
import Getto.Model.Storage exposing ( Storage )
import Getto.Model.Api exposing ( Api )
import Getto.Model.Page exposing ( Page )
import Getto.Model.Project exposing ( Project )
import Getto.Model.Credential exposing ( Credential )

type alias GeneralInfo model account =
  { model
  | application : Application
  , storage     : Storage
  , api         : Api
  , page        : Page
  , project     : Project
  , credential  : Credential account
  , account     : Maybe account
  }
