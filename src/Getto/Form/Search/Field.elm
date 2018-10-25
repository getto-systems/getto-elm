module Getto.Form.Search.Field exposing
  ( Model
  , Sort
  , SortOrder(..)
  , Modify(..)
  , fields
  , string
  , checkList
  , boolList
  , hasSort
  , get
  , values
  , bools
  , options
  , pageTo
  , sortBy
  , set
  , check
  , checkBool
  , sync
  , syncBool
  , modify
  , maybe
  , decodeSearch
  , encode
  , encodeSearch
  , sortQuery
  , pageQuery
  , query
  , encodeFields
  )
import Getto.Form.Edit.Field.Validate as Validate

import Getto.Location as Location
import Getto.Rest as Rest
import Getto.Rest.Query as Query
import Getto.Json as Json

import Json.Decode as Decode
import Json.Decode.Extra exposing ((|:))
import Json.Encode as Encode

import Dict exposing ( Dict )
import List.Extra

import Focus exposing ( Focus, (=>) )

type alias Model a =
  { a
  | fields : Struct
  }

type alias Struct =
  { page : Int
  , sort : Sort
  , pairs : Pairs
  }

type alias Pair = ( String, Spec )
type alias Pairs = Dict String Spec

type Spec
  = String String String
  | CheckList (List String) (List String) Group
  | BoolList (List Bool) Group

type alias Group = Dict String Bool

type alias SortPair = ( String, SortOrder )
type alias SortPairs = Dict String SortOrder

type alias Sort =
  { current : Maybe SortPair
  , default : Maybe SortPair
  , columns : SortPairs
  }

type SortOrder
  = ASC
  | DESC

type Modify
  = Set       String
  | Check     String
  | CheckBool Bool
  | Sync     (List String)
  | SyncBool (List Bool)



fields_ : Focus (Model a) Struct
fields_ = Focus.create .fields (\f model -> { model | fields = model.fields |> f })

pairs_ : Focus Struct Pairs
pairs_ = Focus.create .pairs (\f model -> { model | pairs = model.pairs |> f })

page_ : Focus Struct Int
page_ = Focus.create .page (\f model -> { model | page = model.page |> f })

sort_ : Focus Struct Sort
sort_ = Focus.create .sort (\f model -> { model | sort = model.sort |> f })


fields : List Pair -> List String -> Struct
fields pairs sortColumns =
  let
    sorts = sortColumns |> List.map (\column -> (column, ASC))
    sort =
      let
        default = sorts |> List.head
      in
        { current = default
        , default = default
        , columns = sorts |> Dict.fromList
        }
  in
    { page = 1
    , sort = sort
    , pairs = pairs |> Dict.fromList
    }

string : String -> Spec
string default = String default default

checkList : List String -> List String -> Spec
checkList default options =
  options
  |> toGroup identity default
  |> CheckList default options

boolList : List Bool -> Spec
boolList default =
  [ True, False ]
  |> toGroup toString default
  |> BoolList default

toGroup : (key -> String) -> List key -> List key -> Group
toGroup toKey default =
  List.map (\item -> (item |> toKey, default |> List.member item))
  >> Dict.fromList


hasSort : String -> Sort -> Bool
hasSort name = .columns >> Dict.member name


get : String -> Model a -> String
get name model =
  case model |> find name of
    Just (String _ value) -> value
    _ -> ""

values : String -> Model a -> List String
values name model =
  case model |> find name of
    Just (CheckList _ _ group) -> group |> actives
    _ -> []

bools : String -> Model a -> List Bool
bools name model =
  case model |> find name of
    Just (BoolList _ group) -> group |> actives |> List.map toBool
    _ -> []

options : String -> Model a -> List String
options name model =
  case model |> find name of
    Just (CheckList _ options _) -> options
    _ -> []


find : String -> Model a -> Maybe Spec
find name = pairs >> Dict.get name

pairs : Model a -> Pairs
pairs = .fields >> .pairs

actives : Group -> List String
actives =
  Dict.toList
  >> List.filterMap
    (\(key,value) ->
      if value
        then Just key
        else Nothing
    )


pageTo : Int -> Model a -> Model a
pageTo page = Focus.set (fields_ => page_) page

sortBy : String -> Model a -> Model a
sortBy column = Focus.update (fields_ => sort_) <|
  let
    inverse order =
      case order of
        DESC -> ASC
        ASC -> DESC
  in
    \sort ->
      { sort
      | current =
        case sort.current of
          Nothing -> sort |> sortByDefault column
          Just (current,order) ->
            if current == column
              then Just (current, order |> inverse)
              else sort |> sortByDefault column
      }

sortByDefault : String -> Sort -> Maybe SortPair
sortByDefault column sort =
  case sort.columns |> Dict.get column of
    Just order -> Just (column,order)
    Nothing -> sort.default

sortTo : SortPair -> Model a -> Model a
sortTo (column,order) = Focus.update (fields_ => sort_) <|
  \sort ->
    { sort
    | current =
      case sort.columns |> Dict.get column of
        Just _ -> Just (column,order)
        Nothing -> sort.default
    }


set : String -> String -> Model a -> Model a
set name value = Focus.update (fields_ => pairs_) <|
  Dict.update name <|
    \fieldValue ->
      case fieldValue of
        Just (String default _) -> Just (String default value)
        _ -> fieldValue

check : String -> String -> Model a -> Model a
check name key = Focus.update (fields_ => pairs_) <|
  Dict.update name <|
    \fieldValue ->
      case fieldValue of
        Just (CheckList default options group) ->
          Just (CheckList default options (group |> Dict.update key (Maybe.map not)))
        _ -> fieldValue

checkBool : String -> Bool -> Model a -> Model a
checkBool name key = Focus.update (fields_ => pairs_) <|
  Dict.update name <|
    \fieldValue ->
      case fieldValue of
        Just (BoolList default group) ->
          Just (BoolList default (group |> Dict.update (key |> toString) (Maybe.map not)))
        _ -> fieldValue

sync : String -> List String -> Model a -> Model a
sync name names = Focus.update (fields_ => pairs_) <|
  Dict.update name <|
    \fieldValue ->
      case fieldValue of
        Just (CheckList default options group) ->
          Just (CheckList default options (group |> Dict.toList |> List.map
            (\(groupName,groupValue) ->
              ( groupName
              , names |> List.member groupName
              )
            )
          |> Dict.fromList))
        _ -> fieldValue

syncBool : String -> List Bool -> Model a -> Model a
syncBool name keys =
  let
    names = keys |> List.map toString
  in
    Focus.update (fields_ => pairs_) <|
      Dict.update name <|
        \fieldValue ->
          case fieldValue of
            Just (BoolList default group) ->
              Just (BoolList default (group |> Dict.toList |> List.map
                (\(groupName,groupValue) ->
                  ( groupName
                  , names |> List.member groupName
                  )
                )
              |> Dict.fromList))
            _ -> fieldValue

modify : String -> Modify -> Model a -> Model a
modify name operate model =
  case operate of
    Set       value -> model |> set       name value
    Check     value -> model |> check     name value
    CheckBool value -> model |> checkBool name value
    Sync      value -> model |> sync      name value
    SyncBool  value -> model |> syncBool  name value

maybe : (value -> Model a -> Model a) -> Maybe value -> Model a -> Model a
maybe updater = Maybe.map updater >> Maybe.withDefault identity


toSortSignature : SortPair -> ( String, String )
toSortSignature (column,order) =
  ( column
  , case order of
    ASC  -> "asc"
    DESC -> "desc"
  )

toSortString : SortPair -> String
toSortString = toSortSignature >>
  (\(column,order) ->
    column ++ "." ++ order
  )


keys =
  { page   = "page"
  , sort   = "sort"
  , column = "column"
  , order  = "order"
  , query  = "q"
  , pairs  = "pairs"
  , search = "search"
  , check  = "check"
  , bool   = "bool"
  }

decodeSearch : Location.Search -> Model a -> Model a
decodeSearch search model =
  let
    findQuery name =
      List.Extra.find
        (\(key,_) ->
          key == (keys.query ++ "[" ++ name ++ "]")
        )

    toStringValue name default =
      findQuery name
      >> Maybe.map Tuple.second
      >> Maybe.withDefault default

    toList decoder name default search =
      search
      |> List.filterMap
        (\(key,value) ->
          if key == (keys.query ++ "[" ++ name ++ "][]")
            then value |> decoder
            else Nothing
        )
      |>
        (\vals ->
          if vals |> List.isEmpty
            then default
            else vals
        )

    toStringList = toList (identity >> Just)
    toBoolList   = toList
      (\value ->
        case value of
          "True"  -> Just True
          "False" -> Just False
          _ -> Nothing
      )

    decodeSort =
      search
      |> List.Extra.find (Tuple.first >> (==) keys.sort)
      |> Maybe.andThen
        (\(_,value) ->
          model.fields.sort.columns
          |> Dict.toList
          |> List.filterMap
            (\(column,_) ->
              [ ASC, DESC ]
              |> List.map ((,) column)
              |> List.Extra.find (toSortString >> (==) value)
            )
          |> List.head
          |> Maybe.map sortTo
        )
      |> Maybe.withDefault identity

    decodePage =
      search
      |> List.Extra.find (Tuple.first >> (==) keys.page)
      |> Maybe.andThen
        (\(_,value) ->
          value
          |> String.toInt
          |> Result.toMaybe
          |> Maybe.map pageTo
        )
      |> Maybe.withDefault identity

  in
    model.fields.pairs
    |> Dict.toList
    |> List.foldl
      (\(name,spec) ->
        case spec of
          String    default _   -> search |> toStringValue name default |> set      name
          CheckList default _ _ -> search |> toStringList  name default |> sync     name
          BoolList  default _   -> search |> toBoolList    name default |> syncBool name
      )
      (model |> decodeSort |> decodePage)

encode : Model a -> Encode.Value
encode = encodeFields >> Encode.object

encodeSearch : Model a -> Location.Search
encodeSearch model =
  [ ( keys.query
    , model.fields.pairs
      |> Dict.toList
      |> List.map
        (\(name,spec) ->
          ( name
          , spec |> specQuery
          )
        )
      |> Query.group
    )
  , ( keys.sort, model |> sortQuery )
  , ( keys.page, model |> pageQuery )
  ] |> Query.search

sortQuery : Model a -> Query.SearchValue
sortQuery =
  .fields
  >> .sort
  >> .current
  >> Maybe.map toSortString
  >> Maybe.withDefault ""
  >> Query.string

pageQuery : Model a -> Query.SearchValue
pageQuery =
  .fields
  >> .page
  >> toString
  >> Query.string

query : String -> Model a -> ( String, Query.SearchValue )
query name model =
  ( name
  , model
    |> find name
    |> Maybe.map specQuery
    |> Maybe.withDefault (Query.string "")
  )

specQuery : Spec -> Query.SearchValue
specQuery spec =
  case spec of
    String    _   value -> Query.string value
    CheckList _ _ group -> Query.list (group |> actives)
    BoolList  _   group -> Query.list (group |> actives)

encodeFields : Model a -> Rest.JsonBody
encodeFields model =
  [ ( keys.page,  model.fields.page         |> Encode.int )
  , ( keys.sort,  model.fields.sort.current |> Maybe.map encodeSort |> Maybe.withDefault Encode.null )
  , ( keys.pairs, model.fields.pairs        |> encodePairs )
  ]

encodeSort : SortPair -> Encode.Value
encodeSort = toSortSignature >>
  (\(column,order) ->
    [ ( keys.column, column |> Encode.string )
    , ( keys.order,  order  |> Encode.string )
    ] |> Encode.object
  )

encodePairs : Pairs -> Encode.Value
encodePairs =
  let
    encodeSpec spec =
      case spec of
        String    _   value -> ( keys.search, value |> Encode.string )
        CheckList _ _ group -> ( keys.check,  group |> actives |> List.map Encode.string |> Encode.list )
        BoolList  _   group -> ( keys.bool,   group |> actives |> List.map (toBool >> Encode.bool) |> Encode.list )
  in
    Dict.toList
    >> List.map
      (\(name,spec) ->
        [ ( keys.column, name |> Encode.string )
        , spec |> encodeSpec
        ] |> Encode.object
      )
    >> Encode.list

toBool : String -> Bool
toBool = (==) (True |> toString)
