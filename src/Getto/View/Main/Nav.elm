module Getto.View.Main.Nav exposing ( side, breadcrumb )

import Getto.Env as Env
import Getto.Main as Main
import Getto.Nav as Nav
import Getto.Href as Href
import Getto.Location as Location
import Getto.View.Fa as Fa

import Json.Decode as Decode

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

type alias I18n =
  { title : String -> String
  , menu  : String -> String
  }

breadcrumb : I18n -> Main.Base m info account -> Html msg
breadcrumb i18n model =
  case model.info.menu |> Nav.active of
    Nothing -> H.text ""
    Just active ->
      let
        withSearch item =
          ( item
          , if (item |> Nav.path) == model.info.page.path
              then model.info.page.search
              else []
          )
      in
        H.ul [] <|
          (H.li [] [ H.a [ A.href Env.pageRoot ] [ Fa.solid "home" [] ] ]) ::
          (H.li [] [ H.text (active.group |> i18n.menu) ]) ::
          (active.item |> Nav.ancestors |> List.map
            (withSearch >> link i18n model >> List.singleton >> H.li [])
          )

link : I18n -> Main.Base m info account -> ( Nav.Item, Location.Search ) -> Html msg
link i18n model ((Nav.Item info), search) =
  H.a [ A.href <| (info.path |> Href.href search) ] <| List.concat
    [ [ Fa.solid info.icon ["fw"]
      , H.text " "
      , H.text (info.path |> i18n.title)
      , H.text " "
      ]
    , info.badge |> badgeList
    ]

badgeList : List Nav.Badge -> List (Html msg)
badgeList =
  Nav.sortBadge
  >> List.map
    (\(class,count) ->
      H.em [ A.class <| "badge is-small is-" ++ class ] [ count |> toString |> H.text ]
    )
  >> List.intersperse (H.text " ")

side : I18n -> (Main.Msg account -> msg) -> Html msg -> Main.Base m info account -> Html msg
side i18n mapper content model =
  H.nav []
    [ model |> header
    , content
    , model |> menu i18n mapper
    , model |> footer
    ]

header : Main.Base m info account -> Html msg
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

menu : I18n -> (Main.Msg account -> msg) -> Main.Base m info account -> Html msg
menu i18n mapper model =
  let
    isCollapsedGroup group =
      not group.hasActive &&
      (model.info.menu.collapsed |> List.member group.name)

    isActive (Nav.Item info) =
      info.active ||
      info.hasActive

    toggleMenu group = E.onWithOptions
      "click"
      { stopPropagation = True, preventDefault = True }
      (Decode.succeed (Main.ToggleMenu group.name |> mapper))
  in
    H.section [] <|
      (model.info.menu.groups |> List.map
        (\group ->
          let
            isCollapsed = group |> isCollapsedGroup
            badge = group |> Nav.mergeBadge
          in
            H.ul [ A.class <| if isCollapsed then "is-collapsed" else "" ] <|
              (H.li []
                [ H.b []
                  [ H.a [ A.href "#", (group |> toggleMenu) ] <| List.concat
                    [ [ H.text (group.name |> i18n.menu)
                      , H.text " "
                      ]
                    , badge |> badgeList
                    , [ H.text " "
                      , Fa.solid (if isCollapsed then "caret-left" else "caret-down") []
                      ]
                    ]
                  ]
                ]
              ) ::
              (if isCollapsed then [] else group.items |> List.map
                (\item ->
                  H.li [ A.class <| if isActive item then "is-active" else "" ]
                    [ (item,[]) |> link i18n model ]
                )
              )
        )
      )

footer : Main.Base m info account -> Html msg
footer model = H.footer [] [ H.p [] [ H.text "version : ", H.text model.info.application.version ] ]
