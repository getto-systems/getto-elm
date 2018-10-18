module Getto.View.Main exposing
  ( header
  , footer
  )
import Getto.Main as Main

import Getto.View.Fa as Fa

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

header : Main.Base m info full -> Html msg
header model =
  H.header []
    [ H.p []
      [ H.small [] [ H.text model.info.project.company ]
      , H.wbr [] []
      , H.text " "
      , H.text model.info.project.title
      , H.text " "
      , H.br [] []
      , H.small [] [ H.small [] [ H.text model.info.project.subTitle ] ]
      ]
    ]

footer : Main.Base m info full -> Html msg
footer model =
  H.footer []
    [ H.p []
      [ Fa.copyright, H.text model.info.application.copyright
      ]
    ]
