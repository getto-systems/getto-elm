module Getto.Form.Search.Field exposing
  ( Model
  , Method(..)
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

type alias Pair = ( Method, Spec )
type alias Pairs = Dict Signature Spec
type alias Signature = ( String, String )

type alias DecodeData =
  { column : String
  , method : String
  , search : Maybe String
  , check  : Maybe (List String)
  , bool   : Maybe (List Bool)
  }

type Method
  = EQ            String
  | CONT          String
  | GTEQ          String
  | LTEQ          String
  | IN            String
  | IS            String
  | IS_NULL       String
  | GTEQ_DATETIME String
  | LTEQ_DATETIME String

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
    , pairs =
      pairs
      |> List.map (\(method,spec) -> (method |> toSignature, spec))
      |> Dict.fromList
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


get : Method -> Model a -> String
get method model =
  case model |> find method of
    Just (String _ value) -> value
    _ -> ""

values : Method -> Model a -> List String
values method model =
  case model |> find method of
    Just (CheckList _ _ group) -> group |> actives
    _ -> []

bools : Method -> Model a -> List Bool
bools method model =
  case model |> find method of
    Just (BoolList _ group) -> group |> actives |> List.map toBool
    _ -> []

options : Method -> Model a -> List String
options method model =
  case model |> find method of
    Just (CheckList _ options _) -> options
    _ -> []


find : Method -> Model a -> Maybe Spec
find method = pairs >> Dict.get (method |> toSignature)

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


set : Method -> String -> Model a -> Model a
set method value = Focus.update (fields_ => pairs_) <|
  Dict.update (method |> toSignature) <|
    \fieldValue ->
      case fieldValue of
        Just (String default _) -> Just (String default value)
        _ -> fieldValue

check : Method -> String -> Model a -> Model a
check method key = Focus.update (fields_ => pairs_) <|
  Dict.update (method |> toSignature) <|
    \fieldValue ->
      case fieldValue of
        Just (CheckList default options group) ->
          Just (CheckList default options (group |> Dict.update key (Maybe.map not)))
        _ -> fieldValue

checkBool : Method -> Bool -> Model a -> Model a
checkBool method key = Focus.update (fields_ => pairs_) <|
  Dict.update (method |> toSignature) <|
    \fieldValue ->
      case fieldValue of
        Just (BoolList default group) ->
          Just (BoolList default (group |> Dict.update (key |> toString) (Maybe.map not)))
        _ -> fieldValue

sync : Method -> List String -> Model a -> Model a
sync method names = Focus.update (fields_ => pairs_) <|
  Dict.update (method |> toSignature) <|
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

syncBool : Method -> List Bool -> Model a -> Model a
syncBool method keys =
  let
    names = keys |> List.map toString
  in
    Focus.update (fields_ => pairs_) <|
      Dict.update (method |> toSignature) <|
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

modify : Method -> Modify -> Model a -> Model a
modify method operate model =
  case operate of
    Set       value -> model |> set       method value
    Check     value -> model |> check     method value
    CheckBool value -> model |> checkBool method value
    Sync      value -> model |> sync      method value
    SyncBool  value -> model |> syncBool  method value

maybe : (value -> Model a -> Model a) -> Maybe value -> Model a -> Model a
maybe updater = Maybe.map updater >> Maybe.withDefault identity


toSignature : Method -> Signature
toSignature method =
  case method of
    EQ            column -> ( column, "eq" )
    CONT          column -> ( column, "cont" )
    GTEQ          column -> ( column, "gteq" )
    LTEQ          column -> ( column, "lteq" )
    IN            column -> ( column, "in" )
    IS            column -> ( column, "is" )
    IS_NULL       column -> ( column, "is_null" )
    GTEQ_DATETIME column -> ( column, "gteq_datetime" )
    LTEQ_DATETIME column -> ( column, "lteq_datetime" )

toMethod : Signature -> Maybe Method
toMethod signature =
  case signature of
    ( column, "eq" )             -> Just <| EQ            column
    ( column, "cont" )           -> Just <| CONT          column
    ( column, "gteq" )           -> Just <| GTEQ          column
    ( column, "lteq" )           -> Just <| LTEQ          column
    ( column, "in" )             -> Just <| IN            column
    ( column, "is" )             -> Just <| IS            column
    ( column, "is_null" )        -> Just <| IS_NULL       column
    ( column, "gteq_datetime" )  -> Just <| GTEQ_DATETIME column
    ( column, "lteq_datetime" )  -> Just <| LTEQ_DATETIME column
    _ -> Nothing

toName : Method -> String
toName = toSignature >> Tuple.first

toSortSignature : SortPair -> Signature
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
  , query  = "query"
  , pairs  = "pairs"
  , method = "method"
  , search = "search"
  , check  = "check"
  , bool   = "bool"
  }

decodeSearch : Location.Search -> Model a -> Model a
decodeSearch search model =
  let
    findQuery method =
      List.Extra.find
        (\(key,_) ->
          let
            (column,op) = method |> toSignature
          in
            key == (keys.query ++ "[" ++ column ++ "." ++ op ++ "]")
        )

    toStringValue method default =
      findQuery method
      >> Maybe.map Tuple.second
      >> Maybe.withDefault default

    toList decoder method default search =
      case search |> findQuery method of
        Just _ -> []
        Nothing ->
          search
          |> List.filterMap
            (\(key,value) ->
              let
                (column,op) = method |> toSignature
              in
                if key == (column ++ "." ++ op ++ "[]")
                  then Just (value |> decoder)
                  else Nothing
            )
          |>
            (\vals ->
              if vals |> List.isEmpty
                then default
                else vals
            )

    toStringList = toList identity
    toBoolList   = toList toBool

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
      (\(signature,spec) ->
        signature |> toMethod |> Maybe.map
          (\method ->
            case spec of
              String    default _   -> search |> toStringValue method default |> set      method
              CheckList default _ _ -> search |> toStringList  method default |> sync     method
              BoolList  default _   -> search |> toBoolList    method default |> syncBool method
          )
        |> Maybe.withDefault identity
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
        (\((name,op),spec) ->
          ( name ++ "." ++ op
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
  >> Maybe.map (toSortString >> List.singleton)
  >> Maybe.withDefault []
  >> Query.list

pageQuery : Model a -> Query.SearchValue
pageQuery =
  .fields
  >> .page
  >> toString
  >> Query.string

query : Method -> Model a -> Query.SearchValue
query method =
  find method
  >> Maybe.map specQuery
  >> Maybe.withDefault (Query.string "")

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
      (\((name,op),spec) ->
        [ ( keys.column, name |> Encode.string )
        , ( keys.method, op   |> Encode.string )
        , spec |> encodeSpec
        ] |> Encode.object
      )
    >> Encode.list

toBool : String -> Bool
toBool = (==) (True |> toString)
