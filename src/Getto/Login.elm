module Getto.Login exposing
  ( Base
  )

import Getto.Model.GeneralInfo exposing ( GeneralInfo )

type alias Base m info account =
  { m
  | info : GeneralInfo info account
  }
