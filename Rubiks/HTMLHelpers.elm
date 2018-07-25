module Rubiks.HTMLHelpers exposing (..)


{-|Helper module for RubiksScrambler that produces snippets of HTML,
or interacts directly with TEA.
-}


import Random
import Array exposing (Array)

import Html exposing (Html)
import Html.Attributes as Attr

import Rubiks.Constants as Constants
import Rubiks.Layers as Layers
import Rubiks.Helpers as Helpers


-- COMMANDS

{-| Primarily used to randomly select initial, fake "previously used axis".
-}

randomlySelectAxis : Random.Generator Int
randomlySelectAxis =
  ( Random.int 0 <| (Array.length Constants.axisNames) - 1)


randomlySelectOrientation : Random.Generator Constants.RandomOrientation
randomlySelectOrientation =
  Random.pair
  ( Random.int 0 <| ( Array.length Constants.faceData ) - 2) -- skip blank face
  ( Random.int 0 <| ( Array.length Constants.twistDegrees ) - 1 )


randomMove : Int -> Random.Generator Constants.Move
randomMove cubeSize =
  Random.map3
    Constants.Move
      -- skip zero rotation
      ( Random.int 1 <| (Array.length Constants.twistDegrees) - 1 )
      -- skip previously-used axis, so one fewer option
      ( Random.int 0 <| (Array.length Constants.axisNames) - 2 )
      ( Random.int 0 <| (Layers.numberOfLayers cubeSize) - 1 )


doScrambles : Constants.Model -> Cmd Constants.Msg
doScrambles model =
  Random.generate
  Constants.DoneScrambles
  ( Random.map3
      (,,)
      randomlySelectOrientation
      randomlySelectAxis
      ( Random.list
          model.scrambleTotalMoves
        <|randomMove model.cubeSize )
  )


-- HTML

{-| Generates a list of Html.option elements for a dropdown of cube sizes.
Accepts as input:
The "default" value/index that should be selected,
the value/index for the first cube size,
and a list of strings *describing* each cube size.
The value/index simply gets incremented for each successive cube size.

    cubeShapeOptions 2 0 [ "square", "circle", "triangle"]
      == <option value=0>square
         <option value=1>circle
         <option value=2 selected>triangle
-}

cubeShapeOptions : Int -> Int -> List String -> List (Html msg)
cubeShapeOptions defaultCubeSize index cubeShapes =
  case cubeShapes of
    [] ->
      [] -- bottom out

    head :: rest ->
      Html.option
        [ index
          |> toString
          |> Attr.value
        , Attr.selected <| index == defaultCubeSize
        ]
        [ Html.text <| "Cube Shape: " ++ head ]
      :: cubeShapeOptions defaultCubeSize (index+1) rest


orientationDisplayRow : String -> Int -> List (Html Constants.Msg)
orientationDisplayRow label faceIndex =
  let
    face =
      Array.get faceIndex Constants.faceData

    maybeResult =    
      face
      |>Maybe.map
        ( \face ->
            let
              attr =
                [ Attr.class face ]

            in
              [ Html.dt attr [ Html.text <| label ++ ":" ]
              , Html.dd attr [ Html.text <| face ]
              ]
        )

  in
    Maybe.withDefault [] maybeResult


{-| Converts a raw Orientation data structure into a friendly HTML display

    orientationDisplay orientation == loads of HTML :P
-}


orientationDisplay : Constants.Orientation -> Html Constants.Msg
orientationDisplay orientation =
  Html.dl []
  ( orientationDisplayRow "Front" orientation.front
  ++orientationDisplayRow "Up" orientation.up
  ++orientationDisplayRow "Right" orientation.right
  )


graphicNullCells : Int -> List (Html Constants.Msg)
graphicNullCells count =
  List.repeat count ""
  |>List.map graphicCell


{-|Render a single cell of the scrambled graphic as a <td>.

    graphicCell "Red" =(html)= "<td class='graphicCell Red'></td>"
-}

graphicCell : String -> Html Constants.Msg
graphicCell class =
  Html.td [ Attr.class <| "graphicCell " ++ class ] [ ]


{-| Render a single row on a single face as <td>'s.
This portion of the graphicRender function does not need
to be passed the cubeSize, because the length of the row array
ought to match it anyhow.

    graphicRowRender <|Array.fromList [0,1,2]
    -- gets rendered through TEA into the following HTML:
      """
      <td class="graphicCell Red"></td>
      <td class="graphicCell Blue"></td>
      <td class="graphicCell Yellow"></td>
      """
-}

graphicRowRender : Constants.CubeRowLayout -> List (Html Constants.Msg)
graphicRowRender row =
  row
  |>Array.toList
  |>List.map
    ( \cell ->
        Array.get cell Constants.faceData
        |>Maybe.withDefault ""
        |>graphicCell
    )

{-|Render a "big row". That is, four faces in a horizontal row.
Often this is just 3 blank faces and a real one, but that's
no skin off of this function's nose.

    graphicBigRowRender cubeSize face1 face2 face3 face4
-}

graphicBigRowRender :
  Int
  -> CubeFaceLayout
  -> CubeFaceLayout
  -> CubeFaceLayout
  -> CubeFaceLayout
  -> List (Html Constants.Msg)
graphicBigRowRender cubeSize face1 face2 face3 face4 =
  let
    subRowRender subRow face =
      Array.get subRow face
      |>Maybe.withDefault
        ( Array.repeat cubeSize 6 )
      |> graphicRowRender
  in
    List.range 0 (cubeSize-1)
    |>List.map
      ( \subRow ->
          Html.tr []
          <|subRowRender subRow face1
          ++subRowRender subRow face2
          ++subRowRender subRow face3
          ++subRowRender subRow face4
      )

{-|Render entire scrambled cube graphic, out of smaller pieces.

    graphicRender 2 ( defaultCubeLayout 2) == display default 2x2x2
-}

graphicRender : Int -> Constants.CubeLayout -> List (Html Constants.Msg)
graphicRender cubeSize cubeLayout =
  let
    blank =
      CubeFaceLayout.blankFaceLayout cubeSize
    
    face faceIndex =
      Array.get faceIndex cubeLayout
      |>Maybe.withDefault blank

  in
    graphicBigRowRender cubeSize blank (face 0) blank blank
    ++graphicBigRowRender cubeSize
      ( face 1 )
      ( face 2 )
      ( face 3 )
      ( face 4 )
    ++graphicBigRowRender cubeSize blank (face 5) blank blank
