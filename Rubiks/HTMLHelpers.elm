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


-- COMMANDS

{-| Primarily used to randomly select initial, fake "previously used axis".
-}

randomlySelectAxis : Random.Generator Int
randomlySelectAxis =
  ( Random.int 0 <| (Array.length Constants.axisNames) - 1)


randomlySelectOrientation : Random.Generator Constants.RandomOrientation
randomlySelectOrientation =
  Random.pair
  ( Random.int 0 <| Array.length Constants.faceData )
  ( Random.int 0 <| Array.length Constants.twistDegrees )


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


