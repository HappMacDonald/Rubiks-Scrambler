import Html exposing (Html, div)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Events.Extra exposing (onEnter)
import Array exposing (Array)
import Random
import Rubiks.Layers exposing (numberOfLayers, getLayer)

{-

-- Todo

* Uncaught RangeError: Maximum call stack size exceeded

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


twistDegrees : Array String
twistDegrees =
    Array.fromList ["90", "180", "270"]


axes : Array String
axes =
    Array.fromList ["F", "U", "R"]


-- HELPER FUNCTIONS

{-| Returns result with an error (could not parse to integer, or integer not positive) or with parsed positive integer.
-}

toRangedInteger : String -> Int -> Int -> Result String Int
toRangedInteger inputStr min max =
    let
        input =
            String.trim inputStr
    in
-- We've got to interpret empty as zero so that you can backspace through
-- an entire number and then type new digits and wind up with the expected
-- resulting number. You wind up typing after a zero that has unexpectedly
-- emerged from nowhere, but it then vanishes again after you type your
-- first new digit so everything still works out semantically. 👍
        if String.isEmpty input then
            Ok 0
        else
            input |> String.toInt |> Result.andThen
                (\int ->
                    if int>=min
                        then if int<=max
                            then Ok int
                            else Err
                                <|  "'"
                                ++  inputStr
                                ++  "' must be less than "
                                ++  (toString max)
                                ++  "."
                        else Err
                            <|  "'"
                            ++  inputStr
                            ++  "' must be greater than "
                            ++  (toString min)
                            ++  "."
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
            {   errorStr =
                    ""

            ,   scrambleTotalMoves =
                    14

            ,   scrambleResults = []

            ,   cubeSize =
                    5

            }
    in
        model ! [ doScrambles model ]


-- UPDATE

renderMove : Int -> Int -> (List Move) -> List String
renderMove cubeSize previousAxis uncookedMoves =
  case uncookedMoves of
    [] -> -- bottom out
      [""]
    
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
              <|getLayer 0 cubeSize cookedMove.layer
            )
          ++"("
          ++( Maybe.withDefault "!!"
              <|Array.get cookedMove.twistDegrees twistDegrees
            )
          ++")"
        )
        :: renderMove cubeSize cookedMove.axis remainingMoves


type Msg
  = UpdateScrambleMoves String
  | DoScrambles
  | DoneScrambles ( Int, List Move )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateScrambleMoves inputStr ->
      case toRangedInteger inputStr minimumAllowedMoves maximumAllowedMoves of
        Err error ->
          { model
          | errorStr =
              error
          
          } ! []

        Ok scrambleTotalMoves ->
          { model
          | errorStr =
              ""
          
          , scrambleTotalMoves =
              scrambleTotalMoves
          
          } ! []
    
    DoScrambles ->
      model ! [ doScrambles model ]
    
    DoneScrambles ( previousAxis, moves ) ->
      { model
      | scrambleResults =
          renderMove model.cubeSize previousAxis moves

{- Quick debug in case choices seem strange
      , errorStr
          = (List.head moves |> toString)
          ++" : "
          ++toString previousAxis

-}

      } ! []


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

