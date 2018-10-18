module Getto.Nav exposing
  ( Menu
  , Group
  , Item(..)
  , Info
  , Badge
  , group
  , item
  , sub
  , badge
  , decode
  , encode
  , withGroups
  , activate
  , active
  , ancestors
  , path
  , mergeBadge
  , sortBadge
  )

import Getto.Model.Page exposing ( Page )

import Getto.Href as Href
import Getto.Json as Json

import Json.Encode as Encode
import Json.Decode as Decode

import Dict

type alias Menu =
  { groups : List Group
  , collapsed : List String
  }

type alias Group =
  { name : String
  , items : List Item
  , hasActive : Bool
  }

type Item = Item Info

type alias Info =
  { path : String
  , icon : String
  , active : Bool
  , hasActive : Bool
  , parent : Maybe Item
  , children : List Item
  , badge : List Badge
  }

type alias Badge     = ( String, Int )
type alias BadgeSpec = ( String, Maybe Int )

type alias Active =
  { group : String
  , item : Item
  }

group : String -> List Item -> Group
group name items =
  { name = name
  , items = items
  , hasActive = False
  }

item : String -> String -> Item
item path icon = Item
  { path = path |> Href.chop
  , icon = icon
  , active = False
  , hasActive = False
  , children = []
  , parent = Nothing
  , badge = []
  }

sub : List Item -> Item -> Item
sub items (Item info) =
  Item { info | children = items |> withParent (Item info) }

badge : List BadgeSpec -> Item -> Item
badge badges (Item info) =
  Item
  { info
  | badge = badges |> List.filterMap
    (\(class,count) -> count |> Maybe.map ((,) class))
  }

withParent : Item -> List Item -> List Item
withParent parent = List.map <|
  \(Item info) ->
    Item { info | parent = Just parent }

decode : Encode.Value -> Menu
decode menu =
  { groups = []
  , collapsed = menu |> Json.decodeValue ["collapsed"] (Decode.list Decode.string) |> Maybe.withDefault []
  }

encode : Menu -> Encode.Value
encode menu = Encode.object <|
  [ ("collapsed", menu.collapsed |> List.map Encode.string |> Encode.list)
  ]

withGroups : List Group -> Menu -> Menu
withGroups groups menu = { menu | groups = groups }

activate : Page -> Menu -> Menu
activate page menu =
  let
    activateGroup group =
      let
        items = group.items |> List.map activateItem |> List.map activateParent
      in
        { group
        | items = items
        , hasActive = items |> List.any hasActive
        }

    activateItem (Item info) =
      let
        newItem =
          if info.path == page.path
            then { info | active = True }
            else info
      in
        Item { newItem | children = info.children |> List.map activateItem }

    activateParent (Item info) = Item
      { info
      | hasActive = (Item info) |> hasActive
      , children = info.children |> List.map activateParent
      }

    hasActive (Item info) =
      info.active || (info.children |> List.any hasActive)
  in
    { menu
    | groups = menu.groups |> List.map activateGroup
    }

active : Menu -> Maybe Active
active menu =
  let
    toActive group =
      if group.hasActive
        then group.items |> activeItems |> List.map (\item -> { group = group.name, item = item })
        else []

    activeItems = List.concatMap <|
      \(Item info) ->
        if info.active
          then [Item info]
          else
            if info.hasActive
              then info.children |> activeItems
              else []
  in
    menu.groups |> List.concatMap toActive |> List.head

ancestors : Item -> List Item
ancestors (Item info) =
  List.append (info.parent |> (Maybe.map ancestors) |> Maybe.withDefault []) [Item info]

path : Item -> String
path (Item info) = info.path

mergeBadge : Group -> List Badge
mergeBadge =
  .items
  >> List.foldl
    (\(Item info) acc ->
      info.badge
      |> List.foldl
        (\(class,count) ->
          Dict.update class
            (Maybe.withDefault 0 >> (+) count >> Just)
        )
        acc
    )
    Dict.empty
  >> Dict.toList

sortBadge : List Badge -> List Badge
sortBadge =
  List.sortBy Tuple.first
  >> List.filterMap
    (\(class,count) ->
      if count == 0
        then Nothing
        else Just (class,count)
    )
