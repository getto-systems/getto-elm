module Getto.Login exposing
  ( Base
  )

import Getto.Model.GeneralInfo exposing ( GeneralInfo )

type alias Base m info full =
  { m
  | info : GeneralInfo info full
  }
