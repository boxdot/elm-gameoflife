import Array exposing (Array)
import Random exposing (Generator)
import Time

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events exposing (..)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes as Svg


width = 90
height = 50


main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


-- MODEL

type alias Cell = Bool
type alias Column = Array Cell
type alias Field = Array Column
type alias Model =
  { field : Field
  , running : Bool
  , tick: Time.Time
  }


init : (Model, Cmd Msg)
init =
  let
    model =
      { field = empty width height
      , running = False
      , tick = 100 * Time.millisecond
      }
    cmd = Random.generate NewRandomField random
  in
    (model, cmd)


empty : Int -> Int -> Field
empty width height =
  Array.repeat width (Array.repeat height False)


random : Generator Field
random =
  let
    column =
      Random.list height Random.bool
      |> Random.map Array.fromList
  in
    Random.list width column
    |> Random.map Array.fromList


next : Field -> Field
next field =
  Array.initialize width
  <| \i -> Array.initialize height (becomeAlive field i)


becomeAlive : Field -> Int -> Int -> Cell
becomeAlive field i j =
  let
    is_alive = cell field (i, j)
    numNeighbors = numAliveNeighbors field i j
  in
    case is_alive of
      False -> numNeighbors == 3
      True -> numNeighbors == 2 || numNeighbors == 3


cell : Field -> (Int, Int) -> Cell
cell field (i, j) =
  case Array.get i field |> Maybe.map (Array.get j) of
    Just (Just cell) -> cell
    Just Nothing -> False
    Nothing -> False


numAliveNeighbors : Field -> Int -> Int -> Int
numAliveNeighbors field i j =
  let
    neighbors =
      List.map (cell field)
        [ (i - 1, j - 1)
        , (i    , j - 1)
        , (i + 1, j - 1)
        , (i - 1, j)
        --, (i, j)  Our Cell
        , (i + 1, j)
        , (i - 1, j + 1)
        , (i    , j + 1)
        , (i + 1, j + 1)
        ]
  in
    List.filter identity neighbors
    |> List.length


-- UPDATE

type Msg = Resume | Pause | Next | Reset | NewRandomField Field

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Resume ->
      ({ model | running = True }, Cmd.none)

    Pause ->
      ({ model | running = False }, Cmd.none)

    Next ->
      ({ model | field = next model.field }, Cmd.none)

    Reset ->
      (model, Random.generate NewRandomField random)

    NewRandomField field ->
      ({ model | field = field }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.running of
    True -> Time.every model.tick (\_ -> Next)
    False -> Sub.none


-- VIEW

-- Constants describing dimensions of a cell
cellOpt =
  { height = 10
  , width = 10
  , padding = 1
  }


x : Int -> String
x pos =
  (cellOpt.width + cellOpt.padding) * pos - cellOpt.padding |> toString


y : Int -> String
y pos =
  (cellOpt.height + cellOpt.padding) * pos - cellOpt.padding |> toString


color : Cell -> String
color cell =
  if cell then "black" else "lightgrey"


view : Model -> Html Msg
view model =
  let
    w = x width
    h = y height
    rects =
      Array.indexedMap viewColumn model.field
      |> Array.foldl Array.append Array.empty
      |> Array.toList
  in
    Html.div []
    [ svg
      [ Svg.width w, Svg.height h]
      rects
    , Html.div
      []
      [ Html.button
        [ onClick Resume, Html.disabled model.running ]
        [ text "resume" ]
      , Html.button
        [ onClick Pause, Html.disabled <| not model.running ]
        [ text "pause" ]
      , Html.button [ onClick Next ] [ text "next" ]
      , Html.button [ onClick Reset ] [ text "reset" ]
      ]
    ]


viewColumn : Int -> Column -> Array (Html msg)
viewColumn i column =
  Array.indexedMap (viewCell i) column


viewCell : Int -> Int -> Cell -> Html msg
viewCell i j cell =
  Svg.rect
    [ Svg.x (x i)
    , Svg.y (y j)
    , Svg.width (toString cellOpt.width)
    , Svg.height (toString cellOpt.height)
    , Html.style [ ("fill", color cell) ]
    ]
    []
