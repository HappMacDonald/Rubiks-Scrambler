module RubiksScrambler exposing (..)

import Html exposing (Html, div)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Events.Extra exposing (onEnter)
import Array exposing (Array)
import Random
import Rubiks.Layers exposing (numberOfLayers, getLayer)
import Rubiks.HTMLHelpers as HTMLHelpers
import MyBasics exposing (incrementIf, decrementIf, curryRight)

{-

-- Todo

* Build a visual for what the scrambled cube will look like I suppose?

* randomly choose and then demand colors be assigned to F/U/R 
** Presume plus-yellow color scheme with Red->White->Blue clockwise around a corner.

-}


-- CONSTANTS


minimumAllowedMoves : Int
minimumAllowedMoves =
    0


maximumAllowedMoves : Int
maximumAllowedMoves =
    30


defaultCubeSize : Int
defaultCubeSize = 3

twistDegrees : Array String
twistDegrees =
    Array.fromList ["90", "180", "270"]


axes : Array String
axes =
    Array.fromList ["F", "U", "R"]


cubeShapes : List String
cubeShapes =
  List.map
  ( \cubeSize ->
      cubeSize
      |>toString
      |>List.repeat 3
      |>String.join "x"

  )
  <| List.range 1 10


-- HELPER FUNCTIONS

{-| Just a quick, author-invented guestimate function of optimal number
of scramble moves given a cube size. Ultimately this is (cubesize-0.5)*6,
but I convoluted the formula slightly just to avoid floats.

    defaultScrambles 4 == 21
-}

defaultScrambles : Int -> Int
defaultScrambles cubeSize =
  ( cubeSize*2 - 1 ) * 3


{-| Converts string into an integer, iff it lies between min and max.
String is trimmed first, and blank string is a special case interpreted as 0.

    toRangedInteger "5" -10 10 == Ok 5
    toRangedInteger "5" 10 -10 == Err "min 10 must be no greater than max -10"
    toRangedInteger "  5\t" -10 10 == Ok 5
    toRangedInteger "5 " 8 10 == Err "'5 ' must be no less than 8"
    toRangedInteger " 5 " -10 3 == Err "' 5 ' must be no greater than 3"
    toRangedInteger "" -10 10 == Ok 0
    toRangedInteger "\t \t" -10 10 == Ok 0
    toRangedInteger "" 5 10 == Err "'' must be no less than 5"
-}

toRangedInteger : String -> Int -> Int -> Result String Int
toRangedInteger inputStr min max =
  let
    trimmed =
      String.trim inputStr

    input =
      if String.isEmpty trimmed then
        "0"
      else
        trimmed
        
  in
-- We've got to interpret empty as zero so that you can backspace through
-- an entire number and then type new digits and wind up with the expected
-- resulting number. You wind up typing after a zero that has unexpectedly
-- emerged from nowhere, but it then vanishes again after you type your
-- first new digit so everything still works out semantically. 👍
    if min > max then
      Err
        <|"min "
        ++( toString min )
        ++" must be no greater than max "
        ++( toString max )
        ++"."
    else 
      input |> String.toInt |> Result.andThen
        (\int ->
          if int>=min
            then if int<=max
              then Ok int
              else Err
                <|"'"
                ++inputStr
                ++"' must be no greater than "
                ++(toString max)
                ++"."
            else Err
              <|"'"
              ++inputStr
              ++"' must be no less than "
              ++(toString min)
              ++"."
        )


-- MAIN

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions =
        (\_ -> Sub.none)

    }


-- COMMANDS

type alias Move =
    {   twistDegrees : Int -- index into array of same name
    ,   axis : Int -- index into array of same name
    ,   layer : Int -- index into array of same name
    }


randomlySelectAxis : Random.Generator Int
randomlySelectAxis =
    ( Random.int 0 <| (Array.length axes) - 1) -- randomly select initial, fake "previously used axis"


randomMove : Int -> Random.Generator Move
randomMove cubeSize =
    Random.map3
        Move
            ( Random.int 0 <| (Array.length twistDegrees) - 1 )
            ( Random.int 0 <| (Array.length axes) - 2 ) -- skip previously-used axis
            ( Random.int 0 <| (numberOfLayers cubeSize) - 1 )


doScrambles : Model -> Cmd Msg
doScrambles model =
    Random.generate
    DoneScrambles
    (   Random.pair
            randomlySelectAxis
            ( Random.list
                model.scrambleTotalMoves
              <|randomMove model.cubeSize )
    )


-- MODEL / INIT


type alias Model =
    {   errorStr : String
    ,   scrambleTotalMoves : Int
    ,   scrambleResults : List String
    ,   cubeSize : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            {   errorStr = ""
            ,   scrambleTotalMoves = defaultScrambles defaultCubeSize
            ,   scrambleResults = []
            ,   cubeSize = defaultCubeSize
            }
    in
        model ! [ doScrambles model ]


-- UPDATE

{-| Renders an internal list of moves into a list of strings to display.

    let
      moves =
      [ { axis = 1 -- second available layer of 2
        , layer = 6 -- "23"
        , twistDegrees = 90
        }
      , { axis = 1 -- second available layer of 2
        , layer = 2 -- "2"
        , twistDegrees = 180
        }
      , { axis = 0 -- first available layer of 2
        , layer = 12 -- "34"
        , twistDegrees = 270
        }
      , { axis = -1 -- invalid
        , layer = 12
        , twistDegrees = 270
        }
      , { axis = 0
        , layer = -1 -- invalid
        , twistDegrees = 270
        }
      , { axis = 0
        , layer = 12
        , twistDegrees = 271 -- invalid
        }
      ]
    in
      renderMoves 4 0 moves ==
      [ "D23(90)" -- 0 taken, second available is 2="D"
      , "U2(180)" -- 2 taken, second available is 1="U"
      , "F34(270)" -- 1 taken, first available is 0="F"
      , "!!34(270)"
      , "F!!(270)"
      , "F34(!!)"
      ]
-}
renderMoves : Int -> Int -> (List Move) -> List String
renderMoves cubeSize previousAxis uncookedMoves =
  case uncookedMoves of
    [] -> -- bottom out
      []
    
    uncookedMove :: remainingMoves ->
      let
        cookedMove =
          { uncookedMove
          | axis =
              if uncookedMove.axis >= previousAxis
                then uncookedMove.axis + 1
                else uncookedMove.axis

          }

      in
        ( ( Maybe.withDefault "!!"
            <|Array.get cookedMove.axis axes
          )
          ++( Maybe.withDefault "!!"
              <|getLayer cubeSize cookedMove.layer
            )
          ++"("
          ++( Maybe.withDefault "!!"
              <|Array.get cookedMove.twistDegrees twistDegrees
            )
          ++")"
        )
        :: renderMoves cubeSize cookedMove.axis remainingMoves


type Msg
  = UpdateScrambleMoves String
  | DoScrambles
  | DoneScrambles ( Int, List Move )
  | SizeChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateScrambleMoves inputStr ->
      case toRangedInteger inputStr minimumAllowedMoves maximumAllowedMoves of
        Err error ->
          { model
          | errorStr = error
          } ! []

        Ok scrambleTotalMoves ->
          { model
          | errorStr = ""
          , scrambleTotalMoves = scrambleTotalMoves
          } ! []
    
    DoScrambles ->
      model ! [ doScrambles model ]
    
    DoneScrambles ( previousAxis, moves ) ->
      { model
      | scrambleResults = renderMoves model.cubeSize previousAxis moves

{- Quick debug in case choices seem strange
      , errorStr
          = (List.head moves |> toString)
          ++" : "
          ++toString previousAxis

-}

      } ! []

    SizeChange valueStr ->
      let
        newModel =
          case String.toInt valueStr of
            Err errorStr ->
              { model
              | errorStr = errorStr
{-
                  "I'm sorry, but '"
                  ++valueStr
                  ++"' is not a valid cube size."
-}
              }

            Ok value ->
              { model
              | cubeSize = value
              , scrambleTotalMoves = defaultScrambles value
              }

      in
        newModel ! [ doScrambles newModel ]


-- VIEW

view : Model -> Html Msg
view model =
  div []
  <|[ Html.node "style" []
      [ Html.text """
          html
          { font-family: sans-serif;
            padding: 10px 20px;
          }
          p
          { margin: 10px 0px;
          }
          div.error
          { background-color: red;
            color: white;
            text-weight: bold;
          }
          """
      ]
    , div [ Attr.class "error" ] [ Html.text model.errorStr ]
    , Html.p []
      [
        Html.select [ Events.onInput SizeChange ]
        <|HTMLHelpers.cubeShapeOptions defaultCubeSize 1 cubeShapes
      ]
    , Html.input
      [ Attr.type_ "number"
      , Attr.value <| toString model.scrambleTotalMoves
      , Attr.attribute "min" <| toString minimumAllowedMoves
      , Attr.attribute "max" <| toString maximumAllowedMoves
      , Events.onInput UpdateScrambleMoves
      , onEnter DoScrambles
      ] []
    , Html.button
        [ Events.onClick DoScrambles ]
        [ Html.text "Show " {-++ (toString model.scrambleTotalMoves) ++ " New Scrambles!"-} ]
    ]
  ++  ( List.map (\move -> Html.p [] [ Html.text move ]) model.scrambleResults )

