module Getto.Form.Search.Pager exposing
  ( paging
  , pagers
  )

import Getto.View.Fa as Fa

import Html as H exposing ( Html )
import Html.Attributes as A
import Html.Events as E

type alias Pager =
  { index : Int
  , ellipsis : Bool
  , active : Bool
  }

paging : (Int -> msg) -> ( Int, Int ) -> Html msg
paging msg info =
  case info of
    (_,0) -> H.text ""
    _ ->
      pagers info
      |> List.map (item msg)
      |> H.ul []
      |> List.singleton
      |> H.nav []

item : (Int -> msg) -> Pager -> Html msg
item msg page =
  let
    link = H.a [ A.href "#", E.onClick (msg page.index) ]
  in
    if page.ellipsis
      then H.li [ A.class "is-ellipsis" ] [ link [ Fa.solid "ellipsis-h" [] ] ]
      else
        H.li
          (if page.active
            then [ A.class "is-active" ]
            else []
          )
          [ link [ H.text (page.index |> toString) ] ]

pagers : ( Int, Int ) -> List Pager
pagers (current,pages) =
  let
    window = 3

    page     index = { index = index, ellipsis = False, active = False }
    ellipsis index = { index = index, ellipsis = True,  active = False }
  in
    case pages of
      0 -> [ page 1 ]
      all ->
        let
          left  = current - window
          right = current + window + 1

          (under,remain) =
            List.range 1 all
            |> List.partition (\index -> index < left)

          (view,over) =
            remain
            |> List.partition (\index -> index < right)

          toSide side under =
            case under of
              []      -> []
              [index] -> [ page index ]
              _       -> side
        in
          (List.concat
            [ under |> toSide [ page 1, ellipsis (left - 1) ]
            , view  |> List.map page
            , over  |> toSide [ ellipsis right, page all ]
            ]
          )
          |> List.map (\page -> { page | active = (page.index == current) })
