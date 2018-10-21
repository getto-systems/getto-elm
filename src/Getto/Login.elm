module Getto.Login exposing
  ( Base
  , Info
  )

import Getto.Model.GeneralInfo exposing ( GeneralInfo )

type alias Base m info account =
  { m
  | info : Info info account
  }

type alias Info info account = GeneralInfo info account
