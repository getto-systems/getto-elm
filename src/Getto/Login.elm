module Getto.Login exposing
  ( Base
  )

import Getto.Model.GeneralInfo exposing ( GeneralInfo )

type alias Base m info full limited =
  { m
  | info : GeneralInfo info full limited
  }
