module RubiksScrambler exposing (..)

import Html exposing (Html, div)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Events.Extra exposing (onEnter)
import Rubiks.HTMLHelpers as HTMLHelpers
import Rubiks.Helpers as Helpers
import Rubiks.Constants as Constants
import MyBasics exposing (incrementIf, decrementIf, curryRight)

{-

-- Todo

Status: making CubeFaceLayout module.
* Have solid function, have getter token-functions.

Next: write applier function using tokens.
One possibility: make tokens be:
token : Int(row) -> CubeRowLayout(input row) -> CubeFaceLayout (input face) ->
  (CubeRowLayout(excised row) CubeFaceLayout(modified face))


* Next, work out math to apply scramblemoves onto a CubeLayout
*1> get model to start saving numeric versions of the scramble moves
    in addition to the display versions.
*2> Something to swap 4 row/columns among different faces
*3> Something to rotate a face
*4> And of course the logic to tie that all together into a full,
    generic scrambleMove.

# (Nah) Can Move (and other Constants) become opaque?

-}


-- MAIN

main : Program Never Constants.Model Constants.Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }


-- MODEL / INIT

type alias Model =
  { errorStr : String
  , scrambleTotalMoves : Int
  , scrambleResults : List String
  , cubeSize : Int
  , orientation : Orientation
  , cubeLayout : Array CubeFaceLayout
  }


init : ( Constants.Model, Cmd Constants.Msg )
init =
  let
    model =
      { errorStr = ""
      , scrambleTotalMoves = Helpers.defaultScrambles Constants.defaultCubeSize
      , scrambleResults = []
      , cubeSize = Constants.defaultCubeSize
      , orientation = Constants.defaultOrientation
      , cubeLayout = Helpers.defaultCubeLayout Constants.defaultCubeSize
      }
  in
    model ! [ HTMLHelpers.doScrambles model ]


-- UPDATE

type Msg
  = UpdateScrambleMoves String
  | DoScrambles
  | DoneScrambles RandomPayload
  | SizeChange String


update : Constants.Msg -> Constants.Model -> ( Constants.Model, Cmd Constants.Msg )
update msg model =
  case msg of
    Constants.UpdateScrambleMoves inputStr ->
      let
        input =
          Helpers.toRangedInteger
            inputStr
            Constants.minimumAllowedMoves
            Constants.maximumAllowedMoves
      in
        case input of
          Err error ->
            { model
            | errorStr = error
            } ! []

          Ok scrambleTotalMoves ->
            { model
            | errorStr = ""
            , scrambleTotalMoves = scrambleTotalMoves
            } ! []
    
    Constants.DoScrambles ->
      model ! [ HTMLHelpers.doScrambles model ]
    
    Constants.DoneScrambles ( orientationSeed, previousAxis, moves ) ->
      let
        orientation =
          Helpers.renderOrientation orientationSeed
      
      in
      { model
      | scrambleResults = Helpers.renderMoves model.cubeSize previousAxis moves
      , orientation = orientation
      , cubeLayout =
          Helpers.orientedCubeLayout model.cubeSize orientation
--      , errorStr = toString orientation

{- Quick debug in case choices seem strange
      , errorStr
          = (List.head moves |> toString)
          ++" : "
          ++toString previousAxis

-}

      } ! []

    Constants.SizeChange valueStr ->
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
              , scrambleTotalMoves = Helpers.defaultScrambles value
              , cubeLayout =
                  Helpers.orientedCubeLayout value model.orientation
              }

      in
        newModel ! [ HTMLHelpers.doScrambles newModel ]


-- VIEW

view : Constants.Model -> Html Constants.Msg
view model =
  div []
  <|[ Html.node
        "link"
        [   Attr.rel "stylesheet"
        ,   Attr.href "RubiksScrambler.css"
        ]
        []
    , div [ Attr.class "error" ] [ Html.text model.errorStr ]
    , Html.table [ Attr.class "graphic", Attr.attribute "cellspacing" "1" ]
      <| HTMLHelpers.graphicRender model.cubeSize model.cubeLayout
    , Html.p [] [ Html.text "Hold cube in the following orientation:" ]
    , HTMLHelpers.orientationDisplay model.orientation
    , Html.p []
      [
        Html.select [ Events.onInput Constants.SizeChange ]
        <|HTMLHelpers.cubeShapeOptions Constants.defaultCubeSize 1 Constants.cubeShapes
      ]
    , Html.input
      [ Attr.type_ "number"
      , Attr.value <| toString model.scrambleTotalMoves
      , Attr.attribute "min" <| toString Constants.minimumAllowedMoves
      , Attr.attribute "max" <| toString Constants.maximumAllowedMoves
      , Events.onInput Constants.UpdateScrambleMoves
      , onEnter Constants.DoScrambles
      ] []
    , Html.button
        [ Events.onClick Constants.DoScrambles ]
        [ Html.text "Show" ]
    ]
  ++( List.map (\move -> Html.p [] [ Html.text move ]) model.scrambleResults )

