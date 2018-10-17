module Getto.Moment exposing
  ( nop
  , map
  , batch
  , update
  , andThen
  )

import Focus exposing ( Focus )

type alias Moment model msg = model -> Next model msg
type alias Next   model msg = ( model, Cmd msg )

nop : Moment model msg
nop model = ( model, Cmd.none )

map : (a -> b) -> Next model a -> Next model b
map msg (model,cmd) = ( model, cmd |> Cmd.map msg )

batch : List (model -> Cmd msg) -> Moment model msg
batch cmds model =
  ( model, Cmd.batch (cmds |> List.map (\cmd -> model |> cmd)) )

update : Focus big small -> Moment small msg -> Moment big msg
update prop updater model =
  let
    (small,cmd) = model |> Focus.get prop |> updater
  in
    ( (model |> Focus.set prop small), cmd )

andThen : (before -> Next after msg) -> Next before msg -> Next after msg
andThen updater (model,cmd) =
  let
    (newModel, newCmd) = model |> updater
  in
    ( newModel, Cmd.batch [cmd, newCmd] )
