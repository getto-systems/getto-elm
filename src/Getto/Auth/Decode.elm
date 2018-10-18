module Getto.Auth.Decode exposing
  ( jwt
  )

import Result
import Json.Decode as Decode
import Base64

jwt : Decode.Decoder a -> Decode.Decoder a
jwt decoder =
  Decode.string
  |> Decode.andThen
    (\jwt ->
      let
        padding token =
          let
            length = token |> String.length
            fullLength = length + ((4 - (length % 4)) % 4)
          in
            token |> String.padRight fullLength '='

        token =
          case jwt |> String.split "." of
            _ :: payload :: _ ->
              payload
              |> padding
              |> Base64.decode
              |> Result.mapError toString
              |> Result.mapError ((++) ("base64 error [" ++ payload ++ "]: "))
              |> Result.andThen (Decode.decodeString decoder)
            _ ->
              Err ("jwt mismatch " ++ (jwt |> String.split "." |> String.join(" ")))
      in
        case token of
          Ok val      -> Decode.succeed val
          Err message -> Decode.fail message
    )
