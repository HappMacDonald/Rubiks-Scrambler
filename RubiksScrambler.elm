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

* running through a test series
** left off on Helpers.renderOrientation crap

* Can Move (and other Constants) become opaque?

* randomly choose and then demand colors be assigned to F/U/R 
** Presume plus-yellow color scheme with Red->White->Blue clockwise around a corner.

* Build a visual for what the scrambled cube will look like I suppose?

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


init : ( Constants.Model, Cmd Constants.Msg )
init =
  let
    model =
      { errorStr = ""
      , scrambleTotalMoves = Helpers.defaultScrambles Constants.defaultCubeSize
      , scrambleResults = []
      , cubeSize = Constants.defaultCubeSize
      , orientation = Constants.defaultOrientation
      }
  in
    model ! [ HTMLHelpers.doScrambles model ]


-- UPDATE

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
    
    Constants.DoneScrambles ( orientation, previousAxis, moves ) ->
      { model
      | scrambleResults = Helpers.renderMoves model.cubeSize previousAxis moves
      , orientation = Helpers.renderOrientation orientation

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
              }

      in
        newModel ! [ HTMLHelpers.doScrambles newModel ]


-- VIEW

view : Constants.Model -> Html Constants.Msg
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

